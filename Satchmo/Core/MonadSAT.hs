module Satchmo.Core.MonadSAT
where

import Satchmo.Core.Data (Literal,Clause)

class Monad m => MonadSAT m where

  -- |Generates a fresh literal
  fresh :: m Literal

  -- |Emits a clause
  emit  :: Clause -> m ()

  -- | Emits some note (could be printed by the backend)
  note  :: String -> m ()
