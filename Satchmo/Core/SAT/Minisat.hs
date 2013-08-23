{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
-- |Binding to Minisat solver
module Satchmo.Core.SAT.Minisat
  ( SAT, solve, solveWithTimeout, Config(..), solveConfig, checkAssumptions
  , module Satchmo.Core.MonadSAT)
where

import           Prelude hiding (log)
import           Control.Monad.State
import           System.IO (stderr,hPutStrLn)
import           System.CPUTime (getCPUTime)
import qualified Data.Map.Strict as M
import           Control.Concurrent.MVar (newEmptyMVar,putMVar,takeMVar)
import           Control.Concurrent (killThread,forkIO,threadDelay)
import           Control.Exception (AsyncException,catch)
import           Satchmo.Core.MonadSAT 
import qualified MiniSat as API
import           Satchmo.Core.Data (Literal (..),Clause (..),literal)
import           Satchmo.Core.Decode (Decode (..))
import           Satchmo.Core.Primitive (Primitive,evaluate)
import           Satchmo.Core.Boolean (Boolean (..))
import           Satchmo.Core.Formula (Formula, decodeFormula)

data SATState = SATState { solver          :: ! API.Solver
                         , clauseHistogram :: ! (M.Map Int Int)
                         , numLiterals     :: ! Integer
                         }

emptyState :: API.Solver -> SATState
emptyState solver = SATState solver M.empty 0

newtype SAT a = SAT { runSAT :: StateT SATState IO a }
  deriving (Functor, Monad, MonadState SATState, MonadIO)

instance MonadSAT SAT where
  fresh = do 
    s           <- gets solver
    API.MkLit x <- liftIO $ API.newLit s
    return $ literal True $ fromIntegral x

  emit clause = do
    modify $! \state -> state { 
        clauseHistogram = M.insertWith (+) clauseLength 1 $ clauseHistogram state
      , numLiterals     = fromIntegral clauseLength + numLiterals state
      }
    s <- gets solver
    void $ liftIO $ API.addClause s apiClause
    where
      clauseLength = length $ literals clause
      apiClause    = map toApiLiteral $ literals clause

  note         = liftIO . log
  numVariables = gets solver >>= liftIO . API.minisat_num_vars
  numClauses   = gets solver >>= liftIO . API.minisat_num_clauses

toApiLiteral :: Literal -> API.Lit
toApiLiteral l = ( if isPositive l then id else API.neg ) 
                 $ API.MkLit
                 $ fromIntegral
                 $ variable l

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
solve action = solveConfig [Verbose] $ SAT $ StateT $ \state -> do
                              (a,state') <- runStateT (runSAT action) state
                              return (Just a, state')

data Config = PropagationBudget Int
            | ConflictBudget    Int
            | Verbose
            | forall p . Primitive p => Assumptions [p]

isVerbose :: [Config] -> Bool
isVerbose = \case 
  []        -> False
  Verbose:_ -> True
  _:xs      -> isVerbose xs

getPropagationBudget :: [Config] -> Maybe Int
getPropagationBudget = \case 
  []                      -> Nothing
  (PropagationBudget x):_ -> return x
  _:xs                    -> getPropagationBudget xs

getConflictBudget :: [Config] -> Maybe Int
getConflictBudget = \case 
  []                   -> Nothing
  (ConflictBudget x):_ -> return x
  _:xs                 -> getConflictBudget xs

-- Returns assumption-literals. Returns 'Nothing' if @constant False@ was assumed.
getAssumptions :: [Config] -> Maybe [Literal]
getAssumptions = foldl go (Just [])
  where
   fromAssumption Nothing         _ = Nothing
   fromAssumption (Just literals) a = case evaluate a of
     Left l      -> Just $ literals ++ [l]
     Right True  -> Just literals
     Right False -> Nothing
   
   go literals (Assumptions as') = foldl fromAssumption literals as'
   go literals _                 = literals

setPropagationBudget :: [Config] -> API.Solver -> IO ()
setPropagationBudget config solver = case getPropagationBudget config of
  Nothing -> return ()
  Just b  -> API.minisat_set_prop_budget solver b

setConflictBudget :: [Config] -> API.Solver -> IO ()
setConflictBudget config solver = case getConflictBudget config of
  Nothing -> return ()
  Just b  -> API.minisat_set_conf_budget solver b

isLimited :: [Config] -> Bool
isLimited config = case (getPropagationBudget config, getConflictBudget config) of
  (Nothing, Nothing) -> False
  _                  -> True

solveConfig :: [Config]            -- ^Configuration
            -> SAT (Maybe (SAT a)) -- ^'Maybe' an action in the 'SAT' monad
            -> IO (Maybe a)        -- ^'Maybe' a result
solveConfig config action = API.withNewSolver $ \ solver ->
  let state       = emptyState solver 
      assumptions = getAssumptions config
      verbose     = isVerbose config
  in do
    when verbose $ log $ "Start producing CNF"
    runStateT (runSAT action) state >>= \(result,state') -> case (result,assumptions) of
      (Nothing, _) -> do
        when verbose $ log "Abort because no formula was generated"
        return Nothing
      (_, Nothing) -> do
        when verbose $ log "Abort because of a 'constant False'-assumption"
        return Nothing
      (Just decoder, Just assumptions) -> do
        log "CNF finished"
        when verbose $ do
          log "CNF finished"
          showNumbers state'
          let showHistogram (length,num) = concat [ "#clauses of length "
                                                  , show length, ":\t"
                                                  , show num]
          log $ unlines $ map showHistogram $ M.toList $ clauseHistogram state'
          log $ "Starting solver"

        setPropagationBudget config solver
        setConflictBudget    config solver

        startTime <- getCPUTime
        result    <- case isLimited config of
          False -> do
            result <- API.solve solver $ map toApiLiteral assumptions
            when verbose $ showResult startTime result
            return result

          True -> do
            result <- API.limited_solve solver $ map toApiLiteral assumptions
            when verbose $ showResult startTime result
            return $ result == API.l_True
        if result
          then do
            when verbose $ log $ "Starting decoder"    
            out <- evalStateT (runSAT decoder) state'
            when verbose $ log $ "Decoder finished"    
            return $ Just out
          else return Nothing

checkAssumptions :: [Config] -> SAT (Maybe Bool)
checkAssumptions config = case getAssumptions config of
  Nothing -> return Nothing
  Just as -> do 
    solver <- gets solver
    state  <- get
    liftIO $ do
      when verbose $ showNumbers state

      setPropagationBudget config solver
      setConflictBudget    config solver

      startTime <- getCPUTime
    
      case isLimited config of
        False -> API.solve_simp solver (map toApiLiteral as) False True
             >>= showResultIfVerbose startTime
             >>= return . Just
        True  -> API.limited_solve_simp solver (map toApiLiteral as) False True
             >>= showResultIfVerbose startTime
             >>= \case
                    result | result == API.l_True  -> return $ Just True
                    result | result == API.l_False -> return $ Just False
                    _                              -> return Nothing
  where
    verbose = isVerbose config

    showResultIfVerbose :: Show r => Integer -> r -> IO r
    showResultIfVerbose startTime result = 
      if verbose
      then showResult startTime result >> return result
      else                                return result

showNumbers :: SATState -> IO ()
showNumbers state = do
  v <- API.minisat_num_vars $ solver state
  c <- API.minisat_num_clauses $ solver state
  log $ concat [ "#variables: "  , show v
               , ", #clauses: "  , show c 
               , ", #literals: " , show $ numLiterals state
               ]

log :: String -> IO ()
log = hPutStrLn stderr

showResult :: Show r => Integer -> r -> IO ()
showResult startTime result = do
  endTime  <- getCPUTime

  let diffTime = ( (fromIntegral (endTime - startTime)) / (10^12) ) :: Double

  log $ concat 
    [ "Solver finished in "
    , show diffTime, " seconds (result: " ++ show result ++ ")"
    ]
