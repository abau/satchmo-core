{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
-- |Binding to Minisat solver
module Satchmo.Core.SAT.Minisat
  (SAT, solve, solveWithTimeout, solve', module Satchmo.Core.MonadSAT)
where

import           Control.Monad.State.Strict
import           Control.Applicative (Applicative)
import           System.IO (stderr,hPutStrLn)
import           System.CPUTime (getCPUTime)
import qualified Data.IntMap.Strict as M
import           Control.Concurrent.MVar (newEmptyMVar,putMVar,takeMVar)
import           Control.Concurrent (killThread,forkIO,threadDelay)
import           Control.Exception (AsyncException,catch)
import           Satchmo.Core.MonadSAT 
import qualified MiniSat as API
import           Satchmo.Core.Data (Literal (..),Clause (..),literal)
import           Satchmo.Core.Decode (Decode (..))
import           Satchmo.Core.Boolean (Boolean (..))
import           Satchmo.Core.Formula (Formula, decodeFormula)

data SATState = SATState { solver          :: ! API.Solver
                         , clauseHistogram :: ! (M.IntMap Int)
                         , depthHistogram  :: ! (M.IntMap Int)
                         , numVariables'   :: ! Int
                         , numClauses'     :: ! Int
                         , numLiterals     :: ! Int
                         }

emptyState :: API.Solver -> SATState
emptyState solver = SATState solver M.empty M.empty 0 0 0

newtype SAT a = SAT { runSAT :: StateT SATState IO a }
  deriving (Functor, Applicative, Monad, MonadState SATState, MonadIO)

instance MonadSAT SAT where
  fresh depth = do 
    modify $! \state -> state { numVariables' = numVariables' state + 1 }

    s           <- gets solver
    API.MkLit x <- liftIO $ API.newLit s
    return $ literal True (fromIntegral x) depth

  emit clause = do
    modify $! \state -> state { 
        clauseHistogram = M.insertWith (+) clauseLength 1 $ clauseHistogram state
      , depthHistogram  = foldr (\l -> M.insertWith (+) (depth l) 1)
                                (depthHistogram state)
                                (literals clause)
      , numLiterals     = clauseLength + numLiterals state
      , numClauses'     = numClauses' state + 1
      }
    s <- gets solver
    void $ liftIO $ API.addClause s apiClause
    where
      clauseLength   = length $ literals clause
      apiClause      = map toApiLiteral $ literals clause
      toApiLiteral l = ( if isPositive l then id else API.neg ) 
                       $ API.MkLit
                       $ fromIntegral
                       $ variable l

  note         = liftIO . hPutStrLn stderr
  numVariables = gets numVariables'
  numClauses   = gets numClauses'

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
  let state       = emptyState solver
      density c v = (fromIntegral c) / (fromIntegral v)
  in do
    when verbose $ hPutStrLn stderr $ "Start producing CNF"
    runStateT (runSAT action) state >>= \(result,state') -> case result of
      Nothing -> do
        when verbose $ hPutStrLn stderr "Abort due to known result"
        return Nothing
      Just decoder -> do
        numMinisatVars    <- API.minisat_num_vars    solver
        numMinisatClauses <- API.minisat_num_clauses solver
        when verbose $ do
          hPutStrLn stderr "CNF finished"
          hPutStrLn stderr $ concat [ "#variables: " , show $ numVariables' state'
                                    , ", #clauses: " , show $ numClauses'   state'
                                    , ", #literals: ", show $ numLiterals   state'
                                    , ", clause density: ", show $ density (numClauses'   state')
                                                                           (numVariables' state')
                                    ]
          hPutStrLn stderr $ concat [ "#variables (Minisat): ", show numMinisatVars
                                    , ", #clauses (Minisat): ", show numMinisatClauses
                                    , ", clause density: ", show $ density numMinisatClauses
                                                                           numMinisatVars
                                    ]
          let showHistogram msg (value,num) = concat [msg, show value, ":\t", show num]

          hPutStrLn stderr $ unlines 
                           $ map (showHistogram "#clauses of length ")
                           $ M.toList $ clauseHistogram state'

          hPutStrLn stderr $ unlines 
                           $ map (showHistogram "#literals of depth ")
                           $ M.toList $ depthHistogram state'

          hPutStrLn stderr $ "Starting solver"

        startTime    <- getCPUTime
        solverResult <- API.solve solver []
        endTime      <- getCPUTime

        let diffTime = ( (fromIntegral (endTime - startTime)) / (10^12) ) :: Double

        when verbose $ hPutStrLn stderr $ concat 
          [ "Solver finished in "
          , show diffTime, " seconds (result: " ++ show solverResult ++ ")"
          ]
        if solverResult
          then do
            when verbose $ hPutStrLn stderr $ "Starting decoder"    
            out <- evalStateT (runSAT decoder) state'
            when verbose $ hPutStrLn stderr $ "Decoder finished"    
            return $ Just out
          else return Nothing

