{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module Playground.Bindings where

import Foreign
import Foreign.C.String
import Data.Word

#include "windows_cconv.h"


--{-
foreign import WINDOWS_CCONV unsafe "Clipboard.h GetClipboard"
--foreign import WINDOWS_CCONV unsafe "Clipboard.h _GetClipboard"
 _GetClipboard :: IO CWString

getClipboard :: IO String
getClipboard = _GetClipboard >>= peekCWString
---}

{-
getClipboardData :: Word32 -> IO (Ptr ())
getClipboardData format = _GetClipboardData format
foreign import WINDOWS_CCONV unsafe "windows.h GetClipboardData"
  _GetClipboardData :: Word32 -> IO (Ptr ())
-}
