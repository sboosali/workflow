module Workflow.Derived.Extra
 ( module Workflow.Derived.Extra
 , module X

 -- , module Control.DeepSeq
 -- , module Data.Semigroup
 ) where

-- import Control.DeepSeq (NFData)
-- import Data.Semigroup (Semigroup)

import GHC.Generics as X (Generic)
import Data.Data as X (Data)
import Control.Arrow as X ((>>>))
import Data.Monoid as X ((<>))

import Data.Function as X ((&))


nothing :: (Monad m) => m ()
nothing = return ()

maybe2bool :: Maybe a -> Bool
maybe2bool = maybe False (const True)

either2maybe :: Either e a -> Maybe a
either2maybe = either (const Nothing) Just

either2bool :: Either e a -> Bool
either2bool = either (const False) (const True)
