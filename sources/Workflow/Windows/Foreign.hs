{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module Workflow.Windows.Foreign where
import Workflow.Windows.Types

import Foreign.C

#include "calling_convention.h"

foreign import CALLING_CONVENTION unsafe "Workflow.h GetClipboard"
 c_GetClipboard :: IO CWString

foreign import CALLING_CONVENTION unsafe "Workflow.h SetClipboard"
 c_SetClipboard :: CWString -> IO ()

foreign import CALLING_CONVENTION unsafe "Workflow.h SendUnicodeChar"
 c_SendUnicodeChar :: WCHAR_T -> IO UINT

foreign import CALLING_CONVENTION unsafe "Workflow.h PressKeyDown"
 c_PressKeyDown :: WORD -> IO ()

foreign import CALLING_CONVENTION unsafe "Workflow.h PressKeyUp"
 c_PressKeyUp :: WORD -> IO ()

foreign import CALLING_CONVENTION unsafe "Workflow.h ClickMouseAt"
 c_ClickMouseAt :: Int -> Int -> Int -> DWORD -> DWORD -> IO ()

foreign import CALLING_CONVENTION unsafe "Workflow.h ScrollMouseWheel"
 c_ScrollMouseWheel :: DWORD -> DWORD -> DWORD -> IO ()

foreign import CALLING_CONVENTION unsafe "Workflow.h OpenApplication"
 c_OpenApplication :: CWString -> IO ()

foreign import CALLING_CONVENTION unsafe "Workflow.h OpenUrl"
 c_OpenUrl :: CWString -> IO ()
