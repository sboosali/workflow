{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module Workflow.Bindings where

import Foreign()
import Foreign.C.String
-- import Data.Word

#include "calling_convention.h"


{-|

>>> _Playground
"Playground"

-}
_Workflow :: IO String
_Workflow = c_Workflow >>= peekCString

foreign import CALLING_CONVENTION safe "Workflow.h Workflow"
 c_Workflow :: IO CString

