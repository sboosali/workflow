{-# LANGUAGE NoImplicitPrelude #-}
module Workflow.Extra
 ( module Workflow.Extra
 , module Prelude.Spiros
 , module X
 ) where

import Control.Monad.Catch as X (MonadThrow(..))

import Control.Exception (ErrorCall(..))
import Control.Monad as X

import Prelude.Spiros

failed :: (MonadThrow m) => String -> m a
failed = ErrorCall >>> throwM

