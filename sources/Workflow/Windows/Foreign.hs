{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module Workflow.Windows.Foreign where
import Workflow.Windows.Types

import Foreign.C.Types
import Foreign.C.String (CWString)
import Foreign.Ptr (Ptr)

#include "calling_convention.h"

--TODO unsafe?
--

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

{-|

(reference parameter).

@
BOOL WINAPI GetCursorPos(
  _Out_ LPPOINT lpPoint
);
@

-}
foreign import CALLING_CONVENTION unsafe "Windows.h GetCursorPos"
 c_GetCursorPos :: Ptr POINT -> IO ()

{-|

@
 BOOL WINAPI SetCursorPos(
   _In_ int X,
   _In_ int Y
 );
@

-}
foreign import CALLING_CONVENTION unsafe "Windows.h SetCursorPos"
 c_SetCursorPos :: CInt -> CInt -> IO ()
