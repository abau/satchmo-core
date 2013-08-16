{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
-- |Binding to Minisat solver
module Satchmo.Core.SAT.Minisat
  (SAT, solve, solveWithTimeout, solve', module Satchmo.Core.MonadSAT)
where

import           Control.Monad.State
import           System.IO (stderr,hPutStrLn)
import           System.CPUTime (getCPUTime)
import           Control.Concurrent.MVar (newEmptyMVar,putMVar,takeMVar)
import           Control.Concurrent (killThread,forkIO,threadDelay)
import           Control.Exception (AsyncException,catch)
import           Satchmo.Core.MonadSAT 
import qualified MiniSat as API
import           Satchmo.Core.Data (Literal (..),Clause (..),literal)
import           Satchmo.Core.Decode (Decode (..))
import           Satchmo.Core.Boolean (Boolean (..))
import           Satchmo.Core.Formula (Formula, decodeFormula)

data SATState = SATState { solver   :: API.Solver
                         }

newtype SAT a = SAT { runSAT :: StateT SATState IO a }
  deriving (Functor, Monad, MonadState SATState, MonadIO)

instance MonadSAT SAT where
  fresh = do 
    s           <- gets solver
    API.MkLit x <- liftIO $ API.newLit s
    return $ literal True $ fromIntegral x

  emit clause = do
    s <- gets solver
    void $ liftIO $ API.addClause s apiClause
    where
      apiClause      = map toApiLiteral $ literals clause
      toApiLiteral l = ( if isPositive l then id else API.neg ) 
                       $ API.MkLit
                       $ fromIntegral
                       $ variable l

  note         = liftIO . hPutStrLn stderr
  numVariables = gets solver >>= liftIO . API.minisat_num_vars
  numClauses   = gets solver >>= liftIO . API.minisat_num_clauses

instance Decode SAT Boolean Bool where
  decode b = case b of
    Constant c -> return c
    Boolean  literal -> do 
      s <- gets solver

      let valueOf var = do 
            Just value <- liftIO $ API.modelValue s $ API.MkLit $ fromIntegral var
            return value 

      value <- valueOf $ variable literal
      return $ if isPositive literal then value else not value

instance Decode SAT Formula Bool where
  decode = decodeFormula

solveWithTimeout :: Maybe Int       -- ^Timeout in seconds
                 -> SAT (SAT a)     -- ^Action in the 'SAT' monad
                 -> IO (Maybe a)    -- ^'Maybe' a result
solveWithTimeout mto action = do
  accu <- newEmptyMVar 
  worker <- forkIO $ solve action >>= putMVar accu
  timer <- forkIO $ case mto of
      Just to -> do 
            threadDelay ( 10^6 * to ) 
            killThread worker 
            putMVar accu Nothing
      _  -> return ()
  takeMVar accu `Control.Exception.catch` \ ( _ :: AsyncException ) -> do
      hPutStrLn stderr "caught"
      killThread worker
      killThread timer
      return Nothing

solve :: SAT (SAT a)    -- ^Action in the 'SAT' monad
      -> IO (Maybe a)   -- ^'Maybe' a result
solve action = solve' True $ SAT $ StateT $ \state -> do
                              (a,state') <- runStateT (runSAT action) state
                              return (Just a, state')

solve' :: Bool                -- ^Be verbosely
       -> SAT (Maybe (SAT a)) -- ^Action in the 'SAT' monad
       -> IO (Maybe a)        -- ^'Maybe' a result
solve' verbose action = API.withNewSolver $ \ solver -> 
  let state = SATState solver 
  in do
    when verbose $ hPutStrLn stderr $ "Start producing CNF"
    evalStateT (runSAT action) state >>= \case 
      Nothing -> do
        when verbose $ hPutStrLn stderr "Abort due to known result"
        return Nothing
      Just decoder -> do
        numVars    <- API.minisat_num_vars    solver
        numClauses <- API.minisat_num_clauses solver
        when verbose $ hPutStrLn stderr 
                     $ concat [ "CNF finished (" , "#variables: ", show numVars
                                                 , ", #clauses: "  , show numClauses 
                                                 , ")"]
        when verbose $ hPutStrLn stderr $ "Starting solver"
        startTime <- getCPUTime
        b         <- API.solve solver []
        endTime   <- getCPUTime

        let diffTime = ( (fromIntegral (endTime - startTime)) / (10^12) ) :: Double

        when verbose $ hPutStrLn stderr $ concat 
          [ "Solver finished in "
          , show diffTime, " seconds (result: " ++ show b ++ ")"
          ]
        if b 
          then do
            when verbose $ hPutStrLn stderr $ "Starting decoder"    
            out <- evalStateT (runSAT decoder) $ SATState solver 
            when verbose $ hPutStrLn stderr $ "Decoder finished"    
            return $ Just out
          else return Nothing

