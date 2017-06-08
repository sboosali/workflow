{-# LANGUAGE ForeignFunctionInterface, CPP #-}
{-|

low-level bindings.

-}
module Workflow.Windows.Foreign where
import Workflow.Windows.Types

import Foreign.C.Types
import Foreign.C.String (CWString)
import Foreign.Ptr (Ptr)
import System.IO.Unsafe (unsafePerformIO)


#define SAFETY safe

#include "calling_convention.h"

{-

{-|
@
@

see <>
-}
foreign import CALLING_CONVENTION SAFETY "Windows.h "
 c_ :: IO ()

-}

foreign import CALLING_CONVENTION SAFETY "Workflow.h GetClipboard"
 c_GetClipboard :: IO CWString

foreign import CALLING_CONVENTION SAFETY "Workflow.h SetClipboard"
 c_SetClipboard :: CWString -> IO ()

foreign import CALLING_CONVENTION SAFETY "Workflow.h SendUnicodeChar"
 c_SendUnicodeChar :: WCHAR_T -> IO UINT

foreign import CALLING_CONVENTION SAFETY "Workflow.h SendUnicodeString"
 c_SendUnicodeString :: Int -> LPCWSTR -> Ptr INPUT -> IO UINT

foreign import CALLING_CONVENTION SAFETY "Workflow.h PressKeyDown"
 c_PressKeyDown :: WORD -> IO ()

foreign import CALLING_CONVENTION SAFETY "Workflow.h PressKeyUp"
 c_PressKeyUp :: WORD -> IO ()

foreign import CALLING_CONVENTION SAFETY "Workflow.h ClickMouseAt"
 c_ClickMouseAt :: Int -> Int -> Int -> DWORD -> DWORD -> IO ()

foreign import CALLING_CONVENTION SAFETY "Workflow.h ScrollMouseWheel"
 c_ScrollMouseWheel :: DWORD -> DWORD -> DWORD -> IO ()

foreign import CALLING_CONVENTION SAFETY "Workflow.h OpenApplication"
 c_OpenApplication :: CWString -> IO ()

foreign import CALLING_CONVENTION SAFETY "Workflow.h OpenUrl"
 c_OpenUrl :: CWString -> IO ()

{-|

(reference parameter).

@
BOOL WINAPI GetCursorPos(
  _Out_ LPPOINT lpPoint
);
@

-}
foreign import CALLING_CONVENTION SAFETY "Windows.h GetCursorPos"
 c_GetCursorPos :: Ptr POINT -> IO ()

{-|

@
 BOOL WINAPI SetCursorPos(
   _In_ int X,
   _In_ int Y
 );
@

-}
foreign import CALLING_CONVENTION SAFETY "Windows.h SetCursorPos"
 c_SetCursorPos :: CInt -> CInt -> IO ()

{-|
@
@
-}
foreign import CALLING_CONVENTION SAFETY "Windows.h GetWindowRect"
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
-- foreign import CALLING_CONVENTION SAFETY "Windows.h SetWindowPos"
--  c_SetWindowPos :: IO ()

-- {-|
-- @
-- @
--
-- see <>
-- -}
-- foreign import CALLING_CONVENTION SAFETY "Workflow.h ShowHWND"
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
foreign import CALLING_CONVENTION SAFETY "Windows.h FindWindowW" --TODO FindWindow causes "ld 1: undefined reference"
 c_FindWindow :: CWString -> CWString -> IO VoidStar

{-|

@
DWORD WINAPI GetLastError(void);
@

see
<https://msdn.microsoft.com/en-us/library/windows/desktop/ms679360(v=vs.85).aspx>

see 'SystemErrorCode'

-}
foreign import CALLING_CONVENTION SAFETY "Windows.h GetLastError"
 c_GetLastError :: IO DWORD

{-|

see <https://msdn.microsoft.com/en-us/library/windows/hardware/ff541528(v=vs.85).aspx
Debug Privilege>

-}
foreign import CALLING_CONVENTION SAFETY "Workflow.h EnableDebugPriv"
 c_EnableDebugPriv :: IO BOOL

foreign import CALLING_CONVENTION SAFETY "Workflow.h SizeOfInput"
  c_SizeOfInput :: IO SIZE_T

foreign import CALLING_CONVENTION SAFETY "Workflow.h SizeOfWideChar"
  c_SizeOfWideChar :: IO SIZE_T

-- needed by @.Types@ ??
sizeof_INPUT :: SIZE_T
sizeof_INPUT = unsafePerformIO c_SizeOfInput

{-

ActivateKeyboardLayout
Sets the input locale identifier (formerly called the keyboard layout handle) for the calling thread or the current process. The input locale identifier specifies a locale as well as the physical layout of the keyboard.

BlockInput
Blocks keyboard and mouse input events from reaching applications.

EnableWindow
Enables or disables mouse and keyboard input to the specified window or control. When input is disabled, the window does not receive input such as mouse clicks and key presses. When input is enabled, the window receives all input.


GetActiveWindow
Retrieves the window handle to the active window attached to the calling thread's message queue.

GetAsyncKeyState
Determines whether a key is up or down at the time the function is called, and whether the key was pressed after a previous call to GetAsyncKeyState.

GetFocus
Retrieves the handle to the window that has the keyboard focus, if the window is attached to the calling thread's message queue.

GetKeyboardLayout
Retrieves the active input locale identifier (formerly called the keyboard layout) for the specified thread. If the idThread parameter is zero, the input locale identifier for the active thread is returned.

GetKeyboardLayoutList
Retrieves the input locale identifiers (formerly called keyboard layout handles) corresponding to the current set of input locales in the system. The function copies the identifiers to the specified buffer.

GetKeyboardLayoutName
Retrieves the name of the active input locale identifier (formerly called the keyboard layout).

GetKeyboardState
Copies the status of the 256 virtual keys to the specified buffer.

GetKeyNameText
Retrieves a string that represents the name of a key.

GetKeyState
Retrieves the status of the specified virtual key. The status specifies whether the key is up, down, or toggled (on, off—alternating each time the key is pressed).

GetLastInputInfo
Retrieves the time of the last input event.

IsWindowEnabled
Determines whether the specified window is enabled for mouse and keyboard input.

LoadKeyboardLayout
Loads a new input locale identifier (formerly called the keyboard layout) into the system. Several input locale identifiers can be loaded at a time, but only one per process is active at a time. Loading multiple input locale identifiers makes it possible to rapidly switch between them.

MapVirtualKey
Translates (maps) a virtual-key code into a scan code or character value, or translates a scan code into a virtual-key code.
To specify a handle to the keyboard layout to use for translating the specified code, use the MapVirtualKeyEx function.

MapVirtualKeyEx
Maps a virtual-key code into a scan code or character value, or translates a scan code into a virtual-key code. The function translates the codes using the input language and an input locale identifier.

OemKeyScan
Maps OEMASCII codes 0 through 0x0FF into the OEM scan codes and shift states. The function provides information that allows a program to send OEM text to another program by simulating keyboard input.

RegisterHotKey
Defines a system-wide hot key.

SendInput
Synthesizes keystrokes, mouse motions, and button clicks.

SetActiveWindow
Activates a window. The window must be attached to the calling thread's message queue.

SetFocus
Sets the keyboard focus to the specified window. The window must be attached to the calling thread's message queue.

SetKeyboardState
Copies a 256-byte array of keyboard key states into the calling thread's keyboard input-state table. This is the same table accessed by the GetKeyboardState and GetKeyState functions. Changes made to this table do not affect keyboard input to any other thread.

ToAscii
Translates the specified virtual-key code and keyboard state to the corresponding character or characters. The function translates the code using the input language and physical keyboard layout identified by the keyboard layout handle.
To specify a handle to the keyboard layout to use to translate the specified code, use the ToAsciiEx function.

ToAsciiEx
Translates the specified virtual-key code and keyboard state to the corresponding character or characters. The function translates the code using the input language and physical keyboard layout identified by the input locale identifier.

ToUnicode
Translates the specified virtual-key code and keyboard state to the corresponding Unicode character or characters.
To specify a handle to the keyboard layout to use to translate the specified code, use the ToUnicodeEx function.

ToUnicodeEx
Translates the specified virtual-key code and keyboard state to the corresponding Unicode character or characters.

UnloadKeyboardLayout
Unloads an input locale identifier (formerly called a keyboard layout).

UnregisterHotKey
Frees a hot key previously registered by the calling thread.

VkKeyScanEx
Translates a character to the corresponding virtual-key code and shift state. The function translates the character using the input language and physical keyboard layout identified by the input locale identifier.

-}
