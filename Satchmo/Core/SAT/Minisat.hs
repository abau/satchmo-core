{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Satchmo.Core.SAT.Minisat
  (SAT, solve, solveWithTimeout)
where

import           Control.Monad (void)
import           System.IO (stderr,hPutStrLn)
import           Control.Concurrent.MVar (newEmptyMVar,putMVar,takeMVar)
import           Control.Concurrent (killThread,forkIO,threadDelay)
import           Control.Exception (AsyncException,catch)
import           Satchmo.Core.MonadSAT (MonadSAT (..))
import qualified MiniSat as API
import           Satchmo.Core.Data (Literal (..),Clause (..),literal)
import           Satchmo.Core.Decode (Decode (..))
import           Satchmo.Core.Boolean (Boolean (..))

newtype SAT a = SAT (API.Solver -> IO a)

instance Functor SAT where
  fmap f ( SAT m ) = SAT $ \ s -> fmap f ( m s )

instance Monad SAT where
  return x    = SAT $ const $ return x
  SAT m >>= f = SAT $ \ s -> do x <- m s ; let { SAT n = f x } ; n s

instance MonadSAT SAT where
  fresh = SAT $ \ s -> do 
    API.MkLit x <- API.newLit s
    return $ literal True $ fromIntegral x

  emit clause = SAT $ \ s -> void $ API.addClause s apiClause
    where
      apiClause      = map toApiLiteral $ literals clause
      toApiLiteral l = ( if isPositive l then id else API.neg ) 
                       $ API.MkLit
                       $ fromIntegral
                       $ variable l

  note msg = SAT $ const $ putStrLn msg

instance Decode SAT Boolean Bool where
    decode b = case b of
        Constant c -> return c
        Boolean  literal -> do 
            let valueOf var = SAT $ \ s -> do
                    Just value <- API.modelValue s $ API.MkLit $ fromIntegral var
                    return value 
            value <- valueOf $ variable literal
            return $ if isPositive literal then value else not value

solveWithTimeout :: Maybe Int -> SAT (SAT a) -> IO (Maybe a)
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

solve :: SAT (SAT a) -> IO (Maybe a)
solve ( SAT m ) = API.withNewSolver $ \ s -> do
  hPutStrLn stderr $ "start producing CNF"
  SAT decoder <- m s
  v <- API.minisat_num_vars s
  c <- API.minisat_num_clauses s
  hPutStrLn stderr $ unwords [ "CNF finished", "vars", show v, "clauses", show c ]
  hPutStrLn stderr $ "starting solver"
  b <- API.solve s []
  hPutStrLn stderr $ "solver finished, result: " ++ show b
  if b 
    then do
      hPutStrLn stderr $ "starting decoder"    
      out <- decoder s
      hPutStrLn stderr $ "decoder finished"    
      return $ Just out
    else return Nothing
