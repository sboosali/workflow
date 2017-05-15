module Workflow.Windows.Extra
 ( module Prelude.Spiros
 , module Workflow.Windows.Extra
 ) where

import Foreign (Ptr,nullPtr)

import Prelude.Spiros

isNull :: Ptr a -> Bool
isNull = (== nullPtr)
