module Workflow.Extra
 ( module Workflow.Extra
 , Generic
 , Data
 , NFData
 , module Control.Arrow
 , module Data.Function
 , module Data.Foldable
 ) where

import Control.DeepSeq (NFData)
-- import Data.Semigroup (Semigroup)

import Data.Data (Data)
import GHC.Generics (Generic)
import Control.Arrow ((>>>))
import Data.Function ((&),on)
import Data.Foldable (traverse_)

(-:) :: a -> b -> (a,b)
(-:) = (,)
infix 1 -:
