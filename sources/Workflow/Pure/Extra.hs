module Workflow.Pure.Extra
 ( module Workflow.Pure.Extra

 , module Control.DeepSeq
 , module Data.Semigroup
 , module Control.Monad.Catch

 , module GHC.Generics
 , module Data.Data
 , module Control.Arrow

 , module Data.Function
 ) where

import Control.DeepSeq (NFData)
import Data.Semigroup (Semigroup)
import           Control.Monad.Catch          (MonadThrow(..))

import GHC.Generics (Generic)
import Data.Data (Data)
import Control.Arrow ((>>>))

import Data.Function ((&))


nothing :: (Monad m) => m ()
nothing = return ()

failed :: (MonadThrow m) => String -> m a
failed = throwM . userError
