module Workflow.Extra
 ( module Workflow.Extra
 , module X
 ) where

import Control.DeepSeq as X (NFData)
import Data.Hashable as X (Hashable)
import Data.Semigroup as X (Semigroup)
import Control.Monad.Catch as X (MonadThrow(..))

import Data.Data as X (Data)
import GHC.Generics as X (Generic)
import Control.Arrow as X ((>>>))
import Control.Monad as X ((>=>))
import Data.Function as X ((&),on)
import Data.Foldable as X (traverse_)

import Control.Exception (ErrorCall(..))

(-:) :: a -> b -> (a,b)
(-:) = (,)
infix 1 -:

failed :: (MonadThrow m) => String -> m a
failed = ErrorCall >>> throwM

