{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module Workflow.Windows.Foreign where
import Workflow.Windows.Types

import Foreign.C.Types
import Foreign.C.String (CWString)
import Foreign.Ptr (Ptr)

#include "calling_convention.h"

{-

{-|
@
@

see <>
-}
foreign import CALLING_CONVENTION unsafe "Windows.h "
 c_ :: IO ()

-}

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

{-|
@
@
-}
foreign import CALLING_CONVENTION unsafe "Windows.h GetWindowRect"
 c_GetWindowRect :: VoidStar -> Ptr RECT -> IO ()

-- {-|
--
-- @
-- BOOL WINAPI SetWindowPos(
--   _In_     HWND hWnd,
--   _In_opt_ HWND hWndInsertAfter,
--   _In_     int  X,
--   _In_     int  Y,
--   _In_     int  cx,
--   _In_     int  cy,
--   _In_     UINT uFlags
-- );
-- @
--
-- see <https://msdn.microsoft.com/en-us/library/ms633545.aspx>
--
-- -}
-- foreign import CALLING_CONVENTION unsafe "Windows.h SetWindowPos"
--  c_SetWindowPos :: IO ()

-- {-|
-- @
-- @
--
-- see <>
-- -}
-- foreign import CALLING_CONVENTION unsafe "Workflow.h ShowHWND"
--  c_ShowHWND :: HWND -> IO String
--

{-|

@
HWND WINAPI FindWindowW(
    _In_opt_ LPCWSTR lpClassName,
    _In_opt_ LPCWSTR lpWindowName);
@

see
<https://msdn.microsoft.com/en-us/library/windows/desktop/ms633499(v=vs.85).aspx>

-}
foreign import CALLING_CONVENTION unsafe "Windows.h FindWindowW" --TODO FindWindow causes "ld 1: undefined reference"
 c_FindWindow :: CWString -> CWString -> IO VoidStar

{-|

@
DWORD WINAPI GetLastError(void);
@

see
<https://msdn.microsoft.com/en-us/library/windows/desktop/ms679360(v=vs.85).aspx>

see 'SystemErrorCode'

-}
foreign import CALLING_CONVENTION unsafe "Windows.h GetLastError"
 c_GetLastError :: IO DWORD
