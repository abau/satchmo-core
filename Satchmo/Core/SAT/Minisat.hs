{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
-- |Binding to Minisat solver
module Satchmo.Core.SAT.Minisat
  ( SAT, solve, solveWithTimeout, Config(..), solveConfig, checkAssumptions
  , module Satchmo.Core.MonadSAT)
where

import           Control.Monad (void,when)
import           Control.Monad.IO.Class (MonadIO (..))
import           System.IO (stderr,hPutStrLn)
import           System.CPUTime (getCPUTime)
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

newtype SAT a = SAT (API.Solver -> IO a)

instance Functor SAT where
  fmap f ( SAT m ) = SAT $ \ s -> fmap f ( m s )

instance Monad SAT where
  return x    = SAT $ const $ return x
  SAT m >>= f = SAT $ \ s -> do x <- m s ; let { SAT n = f x } ; n s

instance MonadIO SAT where
  liftIO = SAT . const 

instance MonadSAT SAT where
  fresh = SAT $ \ s -> do 
    API.MkLit x <- API.newLit s
    return $ literal True $ fromIntegral x

  emit clause = SAT $ \ s -> void $ API.addClause s apiClause
    where
      apiClause = map toApiLiteral $ literals clause

  note msg = SAT $ const $ hPutStrLn stderr msg

  numVariables = SAT API.minisat_num_vars
  numClauses   = SAT API.minisat_num_clauses

toApiLiteral :: Literal -> API.Lit
toApiLiteral l = ( if isPositive l then id else API.neg ) 
                 $ API.MkLit
                 $ fromIntegral
                 $ variable l

instance Decode SAT Boolean Bool where
    decode b = case b of
        Constant c -> return c
        Boolean  literal -> do 
            let valueOf var = SAT $ \ s -> do
                    Just value <- API.modelValue s $ API.MkLit $ fromIntegral var
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
solve (SAT m) = solveConfig [Verbose] $ SAT $ \s -> m s >>= return . Just 

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
solveConfig config ( SAT m ) = API.withNewSolver $ \ s -> do
  when verbose $ hPutStrLn stderr $ "Start producing CNF"
  m s >>= \decoder -> case (decoder, getAssumptions config) of
    (Nothing, _) -> do
      when verbose $ hPutStrLn stderr "Abort because no formula was generated"
      return Nothing
    (_, Nothing) -> do
      when verbose $ hPutStrLn stderr "Abort because of a 'constant False'-assumption"
      return Nothing
    (Just (SAT decoder), Just assumptions) -> do
      when verbose $ showNumberOfVarsAndClauses s

      setPropagationBudget config s
      setConflictBudget    config s

      when verbose $ hPutStrLn stderr $ "Starting solver"
      startTime <- getCPUTime

      result <- case isLimited config of
        False -> do
          result <- API.solve s $ map toApiLiteral assumptions
          when verbose $ showResult startTime result
          return result

        True -> do
          result <- API.limited_solve s $ map toApiLiteral assumptions
          when verbose $ showResult startTime result
          return $ result == API.l_True
      if result
        then do
          when verbose $ hPutStrLn stderr $ "Starting decoder"    
          out <- decoder s
          when verbose $ hPutStrLn stderr $ "Decoder finished"    
          return $ Just out
        else return Nothing
  where
    verbose = isVerbose config

checkAssumptions :: [Config] -> SAT (Maybe Bool)
checkAssumptions config = SAT $ \solver -> case getAssumptions config of
  Nothing -> return Nothing
  Just as -> do 
    when verbose $ showNumberOfVarsAndClauses solver

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

showNumberOfVarsAndClauses :: API.Solver -> IO ()
showNumberOfVarsAndClauses solver = do
  v <- API.minisat_num_vars    solver
  c <- API.minisat_num_clauses solver
  hPutStrLn stderr $ concat [ "#variables: "  , show v
                            , ", #clauses: "  , show c 
                            ]

showResult :: Show r => Integer -> r -> IO ()
showResult startTime result = do
  endTime  <- getCPUTime

  let diffTime = ( (fromIntegral (endTime - startTime)) / (10^12) ) :: Double

  hPutStrLn stderr $ concat 
    [ "Solver finished in "
    , show diffTime, " seconds (result: " ++ show result ++ ")"
    ]
