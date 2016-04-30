{-# LANGUAGE ForeignFunctionInterface, CPP, ViewPatterns, RecordWildCards #-}
{-|

all derived in Haskell

-}
module Workflow.Windows.Bindings where
import Workflow.Windows.Types

import Foreign.C
import Data.Char
import Data.Foldable
import Control.Concurrent
import Control.Arrow
import Numeric.Natural

#include "calling_convention.h"


{-
::  -> IO ()
 = c_

foreign import CALLING_CONVENTION unsafe "Workflow.h "
 c_ ::  -> IO ()
-}

getClipboard :: IO String
getClipboard = c_GetClipboard >>= peekCWString

foreign import CALLING_CONVENTION unsafe "Workflow.h GetClipboard"
 c_GetClipboard :: IO CWString


setClipboard :: String -> IO ()
setClipboard s = withCWString s c_SetClipboard

foreign import CALLING_CONVENTION unsafe "Workflow.h SetClipboard"
 c_SetClipboard :: CWString -> IO ()


{-| inserts char-by-char, no delay.

-}
sendText :: String -> IO ()
sendText = traverse_ sendChar -- TODO delay?

{-|

milliseconds

-}
sendTextDelaying :: Int -> String -> IO ()
sendTextDelaying i = traverse_ (\c -> sendChar c >> threadDelay i)


sendChar :: Char -> IO ()
sendChar c = do
 _ <- c_SendUnicodeChar (CWchar (fromIntegral (ord c))) -- cast doesn't overflow, there are ~1,000,000 chars.
 return ()

foreign import CALLING_CONVENTION unsafe "Workflow.h SendUnicodeChar"
 c_SendUnicodeChar :: WCHAR_T -> IO UINT


pressKeychord :: Keyboard key -> [key] -> key -> IO ()
pressKeychord keyboard modifiers key = do
  pressKeyDown keyboard `traverse_` modifiers
  pressKeyDown keyboard key
  pressKeyUp   keyboard key
  pressKeyUp   keyboard `traverse_` (reverse modifiers)

pressKey :: Keyboard key -> key -> IO ()
pressKey keyboard key = do
 pressKeyDown keyboard key
 pressKeyUp   keyboard key

{-|

milliseconds

-}
pressKeyDelaying :: Int -> Keyboard key -> key -> IO ()
pressKeyDelaying milliseconds keyboard key = do
 pressKeyDown keyboard key
 threadDelay milliseconds -- TODO is threadDelay 0 like noop?
 pressKeyUp   keyboard key

pressKeyDown :: Keyboard key -> key -> IO ()
pressKeyDown Keyboard{..} = c_PressKeyDown . fromKey

foreign import CALLING_CONVENTION unsafe "Workflow.h PressKeyDown"
 c_PressKeyDown :: WORD -> IO ()

pressKeyUp :: Keyboard key -> key -> IO ()
pressKeyUp Keyboard{..} = c_PressKeyUp . fromKey

foreign import CALLING_CONVENTION unsafe "Workflow.h PressKeyUp"
 c_PressKeyUp :: WORD -> IO ()


--clickMouse :: Natural -> IO ()

--moveMouseTo ::
--moveMouseTo =

clickMouseAt :: Mouse MOUSEEVENTF -> Point -> Natural -> MOUSEEVENTF -> MOUSEEVENTF -> IO ()
clickMouseAt Mouse{..} Point{..} times down up
 = c_ClickMouseAt (toInt _x) (toInt _y) (toInt times) (fromGizmo down) (fromGizmo up)

foreign import CALLING_CONVENTION unsafe "Workflow.h ClickMouseAt"
  c_ClickMouseAt :: Int -> Int -> Int -> DWORD -> DWORD -> IO ()

{-|

truncates large integral types.

-}
toInt :: (Integral a) => a -> Int
toInt = toInteger >>> (id :: Integer -> Integer) >>> fromIntegral


{-|
TODO windows "apps"
launchApplication :: String -> IO ()
launchApplication s = withCWString s c_LaunchApplication
-}

openApplication :: Application -> IO () -- launchApplication?
openApplication (Application s) = withCWString s c_OpenApplication

foreign import CALLING_CONVENTION unsafe "Workflow.h OpenApplication"
 c_OpenApplication :: CWString -> IO ()


openUrl :: URL -> IO () -- visitURL?
openUrl (URL s) = withCWString s c_OpenUrl

foreign import CALLING_CONVENTION unsafe "Workflow.h OpenUrl"
 c_OpenUrl :: CWString -> IO ()


{-| a raw virtual keyboard.

-}
identityKeyboard :: Keyboard WORD
identityKeyboard = Keyboard id

{-|

virtual key codes from @<WinUser.h>@.

-}
windowsKeyboard :: Keyboard VK
windowsKeyboard = Keyboard{..} where
 fromKey = encodeVK

encodeVK :: VK -> WORD
encodeVK = \case

    VK_BACK           -> 0x08
    VK_TAB            -> 0x09

    VK_CLEAR          -> 0x0C
    VK_RETURN         -> 0x0D
    VK_SHIFT          -> 0x10
    VK_CONTROL        -> 0x11

    VK_MENU           -> 0x12
    VK_PAUSE          -> 0x13
    VK_CAPITAL        -> 0x14

    VK_KANA           -> 0x15
    VK_HANGEUL        -> 0x15  -- old name - should be here for compatibility
    VK_HANGUL         -> 0x15
    VK_JUNJA          -> 0x17
    VK_FINAL          -> 0x18
    VK_HANJA          -> 0x19
    VK_KANJI          -> 0x19
    VK_ESCAPE         -> 0x1B

    VK_CONVERT        -> 0x1C
    VK_NONCONVERT     -> 0x1D
    VK_ACCEPT         -> 0x1E
    VK_MODECHANGE     -> 0x1F

    VK_SPACE          -> 0x20
    VK_PRIOR          -> 0x21
    VK_NEXT           -> 0x22
    VK_END            -> 0x23
    VK_HOME           -> 0x24
    VK_LEFT           -> 0x25
    VK_UP             -> 0x26
    VK_RIGHT          -> 0x27
    VK_DOWN           -> 0x28
    VK_SELECT         -> 0x29
    VK_PRINT          -> 0x2A
    VK_EXECUTE        -> 0x2B
    VK_SNAPSHOT       -> 0x2C
    VK_INSERT         -> 0x2D
    VK_DELETE         -> 0x2E
    VK_HELP           -> 0x2F

    -- VK_0 - VK_9 are the same as ASCII '0' - '9' (0x30 - 0x39)
    VK_0              -> 0x30
    VK_1              -> 0x31
    VK_2              -> 0x32
    VK_3              -> 0x33
    VK_4              -> 0x34
    VK_5              -> 0x35
    VK_6              -> 0x36
    VK_7              -> 0x37
    VK_8              -> 0x38
    VK_9              -> 0x39

    -- 0x40 : unassigned
    -- VK_A - VK_Z are the same as ASCII 'A' - 'Z' (0x41 - 0x5A)
    VK_A              -> 0x41
    VK_B              -> 0x42
    VK_C              -> 0x43
    VK_D              -> 0x44
    VK_E              -> 0x45
    VK_F              -> 0x46
    VK_G              -> 0x47
    VK_H              -> 0x48
    VK_I              -> 0x49
    VK_J              -> 0x4A
    VK_K              -> 0x4B
    VK_L              -> 0x4C
    VK_M              -> 0x4D
    VK_N              -> 0x4E
    VK_O              -> 0x4F
    VK_P              -> 0x50
    VK_Q              -> 0x51
    VK_R              -> 0x52
    VK_S              -> 0x53
    VK_T              -> 0x54
    VK_U              -> 0x55
    VK_V              -> 0x56
    VK_W              -> 0x57
    VK_X              -> 0x58
    VK_Y              -> 0x59
    VK_Z              -> 0x5A

    VK_LWIN           -> 0x5B
    VK_RWIN           -> 0x5C
    VK_APPS           -> 0x5D

    VK_SLEEP          -> 0x5F

    VK_NUMPAD0        -> 0x60
    VK_NUMPAD1        -> 0x61
    VK_NUMPAD2        -> 0x62
    VK_NUMPAD3        -> 0x63
    VK_NUMPAD4        -> 0x64
    VK_NUMPAD5        -> 0x65
    VK_NUMPAD6        -> 0x66
    VK_NUMPAD7        -> 0x67
    VK_NUMPAD8        -> 0x68
    VK_NUMPAD9        -> 0x69
    VK_MULTIPLY       -> 0x6A
    VK_ADD            -> 0x6B
    VK_SEPARATOR      -> 0x6C
    VK_SUBTRACT       -> 0x6D
    VK_DECIMAL        -> 0x6E
    VK_DIVIDE         -> 0x6F
    VK_F1             -> 0x70
    VK_F2             -> 0x71
    VK_F3             -> 0x72
    VK_F4             -> 0x73
    VK_F5             -> 0x74
    VK_F6             -> 0x75
    VK_F7             -> 0x76
    VK_F8             -> 0x77
    VK_F9             -> 0x78
    VK_F10            -> 0x79
    VK_F11            -> 0x7A
    VK_F12            -> 0x7B
    VK_F13            -> 0x7C
    VK_F14            -> 0x7D
    VK_F15            -> 0x7E
    VK_F16            -> 0x7F
    VK_F17            -> 0x80
    VK_F18            -> 0x81
    VK_F19            -> 0x82
    VK_F20            -> 0x83
    VK_F21            -> 0x84
    VK_F22            -> 0x85
    VK_F23            -> 0x86
    VK_F24            -> 0x87

    VK_NUMLOCK        -> 0x90
    VK_SCROLL         -> 0x91

    -- NEC PC-9800 kbd definitions
    VK_OEM_NEC_EQUAL  -> 0x92   -- '=' key on numpad

    -- Fujitsu/OASYS kbd definitions
    VK_OEM_FJ_JISHO   -> 0x92   -- 'Dictionary' key
    VK_OEM_FJ_MASSHOU -> 0x93   -- 'Unregister word' key
    VK_OEM_FJ_TOUROKU -> 0x94   -- 'Register word' key
    VK_OEM_FJ_LOYA    -> 0x95   -- 'Left OYAYUBI' key
    VK_OEM_FJ_ROYA    -> 0x96   -- 'Right OYAYUBI' key

    -- VK_L* & VK_R* - left and right Alt, Ctrl and Shift virtual keys.
    -- Used only as parameters to GetAsyncKeyState() and GetKeyState().
    -- No other API or message will distinguish left and right keys in this way.
    VK_LSHIFT         -> 0xA0
    VK_RSHIFT         -> 0xA1
    VK_LCONTROL       -> 0xA2
    VK_RCONTROL       -> 0xA3
    VK_LMENU          -> 0xA4
    VK_RMENU          -> 0xA5

    VK_BROWSER_BACK        -> 0xA6
    VK_BROWSER_FORWARD     -> 0xA7
    VK_BROWSER_REFRESH     -> 0xA8
    VK_BROWSER_STOP        -> 0xA9
    VK_BROWSER_SEARCH      -> 0xAA
    VK_BROWSER_FAVORITES   -> 0xAB
    VK_BROWSER_HOME        -> 0xAC

    VK_VOLUME_MUTE         -> 0xAD
    VK_VOLUME_DOWN         -> 0xAE
    VK_VOLUME_UP           -> 0xAF
    VK_MEDIA_NEXT_TRACK    -> 0xB0
    VK_MEDIA_PREV_TRACK    -> 0xB1
    VK_MEDIA_STOP          -> 0xB2
    VK_MEDIA_PLAY_PAUSE    -> 0xB3
    VK_LAUNCH_MAIL         -> 0xB4
    VK_LAUNCH_MEDIA_SELECT -> 0xB5
    VK_LAUNCH_APP1         -> 0xB6
    VK_LAUNCH_APP2         -> 0xB7

    VK_OEM_1          -> 0xBA   -- ';:' for US
    VK_OEM_PLUS       -> 0xBB   -- '+' any country
    VK_OEM_COMMA      -> 0xBC   -- ',' any country
    VK_OEM_MINUS      -> 0xBD   -- '-' any country
    VK_OEM_PERIOD     -> 0xBE   -- '.' any country
    VK_OEM_2          -> 0xBF   -- '/?' for US
    VK_OEM_3          -> 0xC0   -- '`~' for US

    VK_OEM_4          -> 0xDB  --  '[{' for US
    VK_OEM_5          -> 0xDC  --  '\|' for US
    VK_OEM_6          -> 0xDD  --  ']}' for US
    VK_OEM_7          -> 0xDE  --  ''"' for US
    VK_OEM_8          -> 0xDF

    -- Various extended or enhanced keyboards
    VK_OEM_AX         -> 0xE1  --  'AX' key on Japanese AX kbd
    VK_OEM_102        -> 0xE2  --  "<>" or "\|" on RT 102-key kbd.
    VK_ICO_HELP       -> 0xE3  --  Help key on ICO
    VK_ICO_00         -> 0xE4  --  00 key on ICO

    VK_PROCESSKEY     -> 0xE5

    VK_ICO_CLEAR      -> 0xE6

    VK_PACKET         -> 0xE7

    VK_OEM_RESET      -> 0xE9
    VK_OEM_JUMP       -> 0xEA
    VK_OEM_PA1        -> 0xEB
    VK_OEM_PA2        -> 0xEC
    VK_OEM_PA3        -> 0xED
    VK_OEM_WSCTRL     -> 0xEE
    VK_OEM_CUSEL      -> 0xEF
    VK_OEM_ATTN       -> 0xF0
    VK_OEM_FINISH     -> 0xF1
    VK_OEM_COPY       -> 0xF2
    VK_OEM_AUTO       -> 0xF3
    VK_OEM_ENLW       -> 0xF4
    VK_OEM_BACKTAB    -> 0xF5

    VK_ATTN           -> 0xF6
    VK_CRSEL          -> 0xF7
    VK_EXSEL          -> 0xF8
    VK_EREOF          -> 0xF9
    VK_PLAY           -> 0xFA
    VK_ZOOM           -> 0xFB
    VK_NONAME         -> 0xFC
    VK_PA1            -> 0xFD
    VK_OEM_CLEAR      -> 0xFE

{-| a raw virtual mouse.

-}
identityMouse :: Mouse DWORD
identityMouse = Mouse id

windowsMouse :: Mouse MOUSEEVENTF
windowsMouse = Mouse{..} where
  fromGizmo = encodeMOUSEEVENTF

{-
windowsWheel :: Mouse MouseWheel
windowsWheel = Mouse{..} where
   fromGizmo =

windowsButton :: Mouse MouseButton
windowsButton = Mouse{..} where
  fromGizmo = \case
-}

encodeMOUSEEVENTF :: MOUSEEVENTF -> DWORD
encodeMOUSEEVENTF =  \case -- TODO pattern?
    MOUSEEVENTF_MOVE        -> 0x0001
    MOUSEEVENTF_LEFTDOWN    -> 0x0002
    MOUSEEVENTF_LEFTUP      -> 0x0004
    MOUSEEVENTF_RIGHTDOWN   -> 0x0008
    MOUSEEVENTF_RIGHTUP     -> 0x0010
    MOUSEEVENTF_MIDDLEDOWN  -> 0x0020
    MOUSEEVENTF_MIDDLEUP    -> 0x0040
    MOUSEEVENTF_XDOWN       -> 0x0080
    MOUSEEVENTF_XUP         -> 0x0100
    MOUSEEVENTF_WHEEL                -> 0x0800
    MOUSEEVENTF_HWHEEL               -> 0x01000
    MOUSEEVENTF_MOVE_NOCOALESCE      -> 0x2000
    MOUSEEVENTF_VIRTUALDESK          -> 0x4000
    MOUSEEVENTF_ABSOLUTE             -> 0x8000

encodeMouseWheel :: MouseWheel -> MOUSEEVENTF
encodeMouseWheel = \case
 VerticalWheel   -> MOUSEEVENTF_WHEEL
 HorizontalWheel -> MOUSEEVENTF_HWHEEL

encodeMouseButton :: MouseButton -> (MOUSEEVENTF,MOUSEEVENTF)
encodeMouseButton = \case
 LeftButton   -> (MOUSEEVENTF_LEFTDOWN   , MOUSEEVENTF_LEFTUP)
 MiddleButton -> (MOUSEEVENTF_MIDDLEDOWN , MOUSEEVENTF_MIDDLEUP)
 RightButton  -> (MOUSEEVENTF_RIGHTDOWN  , MOUSEEVENTF_RIGHTUP)
 XButton      -> (MOUSEEVENTF_XDOWN      , MOUSEEVENTF_XUP)
