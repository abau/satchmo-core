module Satchmo.Core.MonadSAT
  (MonadSAT (..), traced)
where

import Satchmo.Core.Data (Literal,Clause)

class Monad m => MonadSAT m where

  -- |Generates a fresh literal
  fresh :: m Literal

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
  note $ concat [message, " (delta variables: ", show (v2-v1), ", delta clauses: ", show (c2-c1), ")"]
  return result
