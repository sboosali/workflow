module Workflow.Extra
 ( Generic, Data, Monoid
 , NFData, Semigroup
 ) where

import Control.DeepSeq (NFData)
import Data.Semigroup (Semigroup)

import GHC.Generics (Generic)
import Data.Data (Data)
