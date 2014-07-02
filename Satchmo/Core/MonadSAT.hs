module Satchmo.Core.MonadSAT
  (MonadSAT (..), traced)
where

import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Writer as Writer
import qualified Control.Monad.Writer.Strict as WriterStrict
import qualified Control.Monad.State as State
import qualified Control.Monad.State.Strict as StateStrict
import qualified Control.Monad.RWS as RWS
import qualified Control.Monad.RWS.Strict as RWSStrict
import           Control.Monad.Trans (lift)
import           Data.Monoid (Monoid)
import           Satchmo.Core.Data (Literal,Clause)

class Monad m => MonadSAT m where

  -- |@fresh i@ generates a fresh literal at depth @i@
  fresh :: Int -> m Literal

  -- |Emits a clause
  emit :: Clause -> m ()

  -- |Emits some note (could be printed by the backend)
  note :: String -> m ()

  -- |Returns the number of emitted variables
  numVariables :: m Int

  -- |Returns the number of emitted clauses
  numClauses :: m Int

-- |@traced m a@ runs @a@ and prints a message containing @m@, the
-- number of variables emitted by @a@ and the number of clauses
-- emitted by @a@.
traced :: MonadSAT m => String -> m a -> m a
traced message action = do
  v1 <- numVariables
  c1 <- numClauses
  result <- action
  v2 <- numVariables
  c2 <- numClauses
  note $ concat [message, " #variables: ", show (v2-v1), ", #clauses: ", show (c2-c1)]
  return result

instance (MonadSAT m) => MonadSAT (Reader.ReaderT r m) where
  fresh        = lift . fresh
  emit         = lift . emit
  note         = lift . note
  numVariables = lift   numVariables
  numClauses   = lift   numClauses

instance (MonadSAT m, Monoid w) => MonadSAT (Writer.WriterT w m) where
  fresh        = lift . fresh
  emit         = lift . emit
  note         = lift . note
  numVariables = lift   numVariables
  numClauses   = lift   numClauses

instance (MonadSAT m, Monoid w) => MonadSAT (WriterStrict.WriterT w m) where
  fresh        = lift . fresh
  emit         = lift . emit
  note         = lift . note
  numVariables = lift   numVariables
  numClauses   = lift   numClauses

instance (MonadSAT m) => MonadSAT (State.StateT s m) where
  fresh        = lift . fresh
  emit         = lift . emit
  note         = lift . note
  numVariables = lift   numVariables
  numClauses   = lift   numClauses

instance (MonadSAT m) => MonadSAT (StateStrict.StateT s m) where
  fresh        = lift . fresh
  emit         = lift . emit
  note         = lift . note
  numVariables = lift   numVariables
  numClauses   = lift   numClauses

instance (MonadSAT m, Monoid w) => MonadSAT (RWS.RWST r w s m) where
  fresh        = lift . fresh
  emit         = lift . emit
  note         = lift . note
  numVariables = lift   numVariables
  numClauses   = lift   numClauses

instance (MonadSAT m, Monoid w) => MonadSAT (RWSStrict.RWST r w s m) where
  fresh        = lift . fresh
  emit         = lift . emit
  note         = lift . note
  numVariables = lift   numVariables
  numClauses   = lift   numClauses
