{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module Workflow.Bindings where

import Foreign()
import Foreign.C.String
-- import Data.Word

#include "calling_convention.h"


getClipboard :: IO String
getClipboard = c_GetClipboard >>= peekCWString

foreign import CALLING_CONVENTION unsafe "Workflow.h GetClipboard"
 --foreign import WINDOWS_CCONV unsafe "Clipboard.h _GetClipboard"
 c_GetClipboard :: IO CWString
