{-# LANGUAGE GeneralizedNewtypeDeriving, PatternSynonyms, DeriveDataTypeable, DeriveGeneric, RecordWildCards, EmptyDataDecls #-}
{-|

Uppercased types are
<https://msdn.microsoft.com/en-us/library/windows/desktop/aa383751(v=vs.85).aspx
Windows Data Types>

-}
module Workflow.Windows.Types where
import Workflow.Windows.Extra

import Foreign.CStorable

import Foreign
import Foreign.C.Types
import Foreign.C.String
import GHC.Exts
import Data.Ix (Ix)

--TODO foreign-var-0.1

type LPCWSTR = CWString

type WCHAR_T = CWchar

type UINT = Word32 -- CUInt

type WORD = Word16

type DWORD = Word32

-- | (may overflow)
toDWORD :: (Integral a) => a -> DWORD
toDWORD = fromIntegral

{-|

@
LONG
A 32-bit signed integer. The range is –2147483648 through 2147483647 decimal.
This type is declared in WinNT.h as follows:
typedef long LONG;
@

-}
type LONG = Int32

-- | @void*@
type VoidStar = Ptr ()

{-| an abstract handle to a window.

-}
-- data HWND = HWND
-- newtype HWND = HWND (Ptr ())
-- type HWND = Ptr ()
newtype HWND = HWND VoidStar

getHWND :: HWND -> VoidStar
getHWND (HWND p) = p

{-|

-}
type BOOL = CInt

{-|

see
<https://msdn.microsoft.com/en-us/library/windows/desktop/ms681381(v=vs.85).aspx
System Error Codes>

-}
newtype SystemErrorCode = SystemErrorCode DWORD
 deriving (Bounded, Enum, Eq, Integral, Data, Num, Ord, Read, Real, Show, Ix, FiniteBits, Bits, Storable)

{-|

see
<https://msdn.microsoft.com/en-us/library/windows/desktop/ms646273(v=vs.85).aspx Mouse Events>

-}
newtype MOUSEEVENTF = MOUSEEVENTF DWORD
 deriving (Bounded, Enum, Eq, Integral, Data, Num, Ord, Read, Real, Show, Ix, FiniteBits, Bits, Storable)

getMOUSEEVENTF :: MOUSEEVENTF -> DWORD
getMOUSEEVENTF (MOUSEEVENTF n) = n
-- manual accessor doesn't pollute Show instance

{-|

see
<https://msdn.microsoft.com/en-us/library/windows/desktop/dd375731(v=vs.85).aspx Virtual-Key Codes>

(no display brightness, no @Fn@ modifier key).

-}
newtype VK = VK WORD
 deriving (Bounded, Enum, Eq, Integral, Data, Num, Ord, Read, Real, Show, Ix, FiniteBits, Bits, Storable)

getVK :: VK -> WORD
getVK (VK n) = n
-- manual accessor doesn't pollute Show instance

--------------------------------------------------------------------------------

-- type LPPOINT = Ptr POINT

-- type POINT =
--   ( LONG  -- x
--   , LONG  -- y
--   )
--
-- pattern POINT x y = (x,y)

-- {-|
--
-- TODO vinyl record
-- GetCursorPos
--
--  with malloc $ \p -> do
--   GetCursorPos p
--   sequence [peek p 0, peek p 4]
--
-- -}

{-|

@
struct POINT
{
    LONG  x;
    LONG  y;
}
@

-}
data POINT = POINT
 { xPOINT :: LONG
 , yPOINT :: LONG
 } deriving (Show,Read,Eq,Ord,Generic,Data)--,NFData,Semigroup,Monoid)

instance CStorable POINT
instance Storable  POINT where
 peek      = cPeek
 poke      = cPoke
 alignment = cAlignment
 sizeOf    = cSizeOf

--------------------------------------------------------------------------------

{-|

@
struct RECT
{
    LONG    left;
    LONG    top;
    LONG    right;
    LONG    bottom;
}
@

-}
data RECT = RECT
 { leftRECT   :: LONG
 , topRECT    :: LONG
 , rightRECT  :: LONG
 , bottomRECT :: LONG
 } deriving (Show,Read,Eq,Ord,Generic,Data)--,NFData,Semigroup,Monoid)

instance CStorable RECT
instance Storable  RECT where
 peek      = cPeek
 poke      = cPoke
 alignment = cAlignment
 sizeOf    = cSizeOf

--------------------------------------------------------------------------------

newtype Application = Application String
 deriving (Show,IsString,Read,Eq,Ord,Generic,Data)--,NFData,Semigroup,Monoid)

-- | (accessor)
getApplication :: Application -> String
getApplication (Application s) = s

newtype URL = URL String
 deriving (Show,IsString,Read,Eq,Ord,Generic,Data)--,NFData,Semigroup,Monoid)

-- | (accessor)
getURL :: URL -> String
getURL (URL s) = s

data MouseButton
 = LeftButton
 | MiddleButton
 | RightButton
 | XButton
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,Data)--,NFData,Semigroup,Monoid)

data MouseScroll
  = ScrollTowards -- ScrollUp (from user)
  | ScrollAway -- ScrollDown (from user)
  | ScrollLeft
  | ScrollRight
  deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,Data)--,NFData,Semigroup,Monoid)

{-|

Non-unique. e.g. open two blank (chrome) browser windows, both will match:

@
Window{..}
 where
 windowExecutable = "chrome.exe"
 windowClass      = "Chrome_WidgetWin_1"
 windowTitle      = "New Tab - Google Chrome"
@

-}
data Window = Window
 { windowExecutable :: String
 , windowClass      :: String
 , windowTitle      :: String
 } deriving (Show) --TODO

-- | only the 'windowTitle'
instance IsString Window where -- TODO Maybe String? Vinyl? Rec Id and Rec Maybe.
 fromString = aWindowTitle

aWindowExecutable :: String -> Window
aWindowExecutable s = Window{..}
  where
  windowExecutable = s
  windowClass      = ""
  windowTitle      = ""

aWindowClass :: String -> Window
aWindowClass s = Window{..}
  where
  windowExecutable = ""
  windowClass      = s
  windowTitle      = ""

aWindowTitle :: String -> Window
aWindowTitle s = Window{..}
  where
  windowExecutable = ""
  windowClass      = ""
  windowTitle      = s

--------------------------------------------------------------------------------

pattern ERROR_INVALID_WINDOW_HANDLE :: SystemErrorCode
pattern ERROR_INVALID_WINDOW_HANDLE = 1400

--------------------------------------------------------------------------------

-- | mouse move
pattern MOUSEEVENTF_MOVE :: MOUSEEVENTF
pattern MOUSEEVENTF_MOVE = 0x0001

-- | left button down
pattern MOUSEEVENTF_LEFTDOWN :: MOUSEEVENTF
pattern MOUSEEVENTF_LEFTDOWN = 0x0002

-- | left button up
pattern MOUSEEVENTF_LEFTUP :: MOUSEEVENTF
pattern MOUSEEVENTF_LEFTUP = 0x0004

-- | right button down
pattern MOUSEEVENTF_RIGHTDOWN :: MOUSEEVENTF
pattern MOUSEEVENTF_RIGHTDOWN = 0x0008

-- | right button up
pattern MOUSEEVENTF_RIGHTUP :: MOUSEEVENTF
pattern MOUSEEVENTF_RIGHTUP = 0x0010

-- | middle button down
pattern MOUSEEVENTF_MIDDLEDOWN :: MOUSEEVENTF
pattern MOUSEEVENTF_MIDDLEDOWN = 0x0020

-- | middle button up
pattern MOUSEEVENTF_MIDDLEUP :: MOUSEEVENTF
pattern MOUSEEVENTF_MIDDLEUP = 0x0040

-- | x button down i.e. TODO
pattern MOUSEEVENTF_XDOWN :: MOUSEEVENTF
pattern MOUSEEVENTF_XDOWN = 0x0080

-- | x button up
pattern MOUSEEVENTF_XUP :: MOUSEEVENTF
pattern MOUSEEVENTF_XUP = 0x0100

-- | vertical wheel rolled
pattern MOUSEEVENTF_WHEEL :: MOUSEEVENTF
pattern MOUSEEVENTF_WHEEL = 0x0800

-- | horizontal wheel rolled
pattern MOUSEEVENTF_HWHEEL :: MOUSEEVENTF
pattern MOUSEEVENTF_HWHEEL = 0x01000

-- | do not coalesce mouse moves i.e. TODO
pattern MOUSEEVENTF_MOVE_NOCOALESCE :: MOUSEEVENTF
pattern MOUSEEVENTF_MOVE_NOCOALESCE = 0x2000

-- | map to entire virtual desktop i.e. TODO
pattern MOUSEEVENTF_VIRTUALDESK :: MOUSEEVENTF
pattern MOUSEEVENTF_VIRTUALDESK = 0x4000

-- | absolute move i.e. TODO
pattern MOUSEEVENTF_ABSOLUTE :: MOUSEEVENTF
pattern MOUSEEVENTF_ABSOLUTE = 0x8000

--------------------------------------------------------------------------------

-- |
pattern VK_BACK :: VK
pattern VK_BACK = 0x08

-- |
pattern VK_TAB :: VK
pattern VK_TAB = 0x09

-- |
pattern VK_CLEAR :: VK
pattern VK_CLEAR = 0x0C

-- |
pattern VK_RETURN :: VK
pattern VK_RETURN = 0x0D
-- |
pattern VK_SHIFT :: VK
pattern VK_SHIFT = 0x10
-- |
pattern VK_CONTROL :: VK
pattern VK_CONTROL = 0x11

-- | i.e. @Alt@
pattern VK_MENU :: VK
pattern VK_MENU = 0x12
-- |
pattern VK_PAUSE :: VK
pattern VK_PAUSE = 0x13
-- | @CapsLock@
pattern VK_CAPITAL :: VK
pattern VK_CAPITAL = 0x14

-- |
pattern VK_KANA :: VK
pattern VK_KANA = 0x15
-- | "old name - should be here for compatibility"
pattern VK_HANGEUL :: VK
pattern VK_HANGEUL = 0x15
-- |
pattern VK_HANGUL :: VK
pattern VK_HANGUL = 0x15
-- |
pattern VK_JUNJA :: VK
pattern VK_JUNJA = 0x17
-- |
pattern VK_FINAL :: VK
pattern VK_FINAL = 0x18
-- |
pattern VK_HANJA :: VK
pattern VK_HANJA = 0x19
-- |
pattern VK_KANJI :: VK
pattern VK_KANJI = 0x19
-- |
pattern VK_ESCAPE :: VK
pattern VK_ESCAPE = 0x1B

-- |
pattern VK_CONVERT :: VK
pattern VK_CONVERT = 0x1C
-- |
pattern VK_NONCONVERT :: VK
pattern VK_NONCONVERT = 0x1D
-- |
pattern VK_ACCEPT :: VK
pattern VK_ACCEPT = 0x1E
-- |
pattern VK_MODECHANGE :: VK
pattern VK_MODECHANGE = 0x1F

-- |
pattern VK_SPACE :: VK
pattern VK_SPACE = 0x20
-- | @PgUp@
pattern VK_PRIOR :: VK
pattern VK_PRIOR = 0x21
-- | @PgDn@
pattern VK_NEXT :: VK
pattern VK_NEXT = 0x22
-- |
pattern VK_END :: VK
pattern VK_END = 0x23
-- |
pattern VK_HOME :: VK
pattern VK_HOME = 0x24
-- |
pattern VK_LEFT :: VK
pattern VK_LEFT = 0x25
-- |
pattern VK_UP :: VK
pattern VK_UP = 0x26
-- |
pattern VK_RIGHT :: VK
pattern VK_RIGHT = 0x27
-- |
pattern VK_DOWN :: VK
pattern VK_DOWN = 0x28
-- |
pattern VK_SELECT :: VK
pattern VK_SELECT = 0x29
-- |
pattern VK_PRINT :: VK
pattern VK_PRINT = 0x2A
-- |
pattern VK_EXECUTE :: VK
pattern VK_EXECUTE = 0x2B
-- |
pattern VK_SNAPSHOT :: VK
pattern VK_SNAPSHOT = 0x2C
-- |
pattern VK_INSERT :: VK
pattern VK_INSERT = 0x2D
-- |
pattern VK_DELETE :: VK
pattern VK_DELETE = 0x2E
-- |
pattern VK_HELP :: VK
pattern VK_HELP = 0x2F

-- | 'VK_0' to 'VK_9' are the same as ASCII @0@ to @9@ (@0x30@ to @0x39@)
pattern VK_0 :: VK
pattern VK_0 = 0x30
-- |
pattern VK_1 :: VK
pattern VK_1 = 0x31
-- |
pattern VK_2 :: VK
pattern VK_2 = 0x32
-- |
pattern VK_3 :: VK
pattern VK_3 = 0x33
-- |
pattern VK_4 :: VK
pattern VK_4 = 0x34
-- |
pattern VK_5 :: VK
pattern VK_5 = 0x35
-- |
pattern VK_6 :: VK
pattern VK_6 = 0x36
-- |
pattern VK_7 :: VK
pattern VK_7 = 0x37
-- |
pattern VK_8 :: VK
pattern VK_8 = 0x38
-- |
pattern VK_9 :: VK
pattern VK_9 = 0x39

-- 0x40 : unassigned

-- | 'VK_A' to 'VK_Z' are the same as ASCII @A@ to @Z@ (@0x41@ to @0x5A@)
pattern VK_A :: VK
pattern VK_A = 0x41
-- |
pattern VK_B :: VK
pattern VK_B = 0x42
-- |
pattern VK_C :: VK
pattern VK_C = 0x43
-- |
pattern VK_D :: VK
pattern VK_D = 0x44
-- |
pattern VK_E :: VK
pattern VK_E = 0x45
-- |
pattern VK_F :: VK
pattern VK_F = 0x46
-- |
pattern VK_G :: VK
pattern VK_G = 0x47
-- |
pattern VK_H :: VK
pattern VK_H = 0x48
-- |
pattern VK_I :: VK
pattern VK_I = 0x49
-- |
pattern VK_J :: VK
pattern VK_J = 0x4A
-- |
pattern VK_K :: VK
pattern VK_K = 0x4B
-- |
pattern VK_L :: VK
pattern VK_L = 0x4C
-- |
pattern VK_M :: VK
pattern VK_M = 0x4D
-- |
pattern VK_N :: VK
pattern VK_N = 0x4E
-- |
pattern VK_O :: VK
pattern VK_O = 0x4F
-- |
pattern VK_P :: VK
pattern VK_P = 0x50
-- |
pattern VK_Q :: VK
pattern VK_Q = 0x51
-- |
pattern VK_R :: VK
pattern VK_R = 0x52
-- |
pattern VK_S :: VK
pattern VK_S = 0x53
-- |
pattern VK_T :: VK
pattern VK_T = 0x54
-- |
pattern VK_U :: VK
pattern VK_U = 0x55
-- |
pattern VK_V :: VK
pattern VK_V = 0x56
-- |
pattern VK_W :: VK
pattern VK_W = 0x57
-- |
pattern VK_X :: VK
pattern VK_X = 0x58
-- |
pattern VK_Y :: VK
pattern VK_Y = 0x59
-- |
pattern VK_Z :: VK
pattern VK_Z = 0x5A

-- |
pattern VK_LWIN :: VK
pattern VK_LWIN = 0x5B
-- |
pattern VK_RWIN :: VK
pattern VK_RWIN = 0x5C
-- |
pattern VK_APPS :: VK
pattern VK_APPS = 0x5D

-- |
pattern VK_SLEEP :: VK
pattern VK_SLEEP = 0x5F

-- |
pattern VK_NUMPAD0 :: VK
pattern VK_NUMPAD0 = 0x60
-- |
pattern VK_NUMPAD1 :: VK
pattern VK_NUMPAD1 = 0x61
-- |
pattern VK_NUMPAD2 :: VK
pattern VK_NUMPAD2 = 0x62
-- |
pattern VK_NUMPAD3 :: VK
pattern VK_NUMPAD3 = 0x63
-- |
pattern VK_NUMPAD4 :: VK
pattern VK_NUMPAD4 = 0x64
-- |
pattern VK_NUMPAD5 :: VK
pattern VK_NUMPAD5 = 0x65
-- |
pattern VK_NUMPAD6 :: VK
pattern VK_NUMPAD6 = 0x66
-- |
pattern VK_NUMPAD7 :: VK
pattern VK_NUMPAD7 = 0x67
-- |
pattern VK_NUMPAD8 :: VK
pattern VK_NUMPAD8 = 0x68
-- |
pattern VK_NUMPAD9 :: VK
pattern VK_NUMPAD9 = 0x69
-- |
pattern VK_MULTIPLY :: VK
pattern VK_MULTIPLY = 0x6A
-- |
pattern VK_ADD :: VK
pattern VK_ADD = 0x6B
-- |
pattern VK_SEPARATOR :: VK
pattern VK_SEPARATOR = 0x6C
-- |
pattern VK_SUBTRACT :: VK
pattern VK_SUBTRACT = 0x6D
-- |
pattern VK_DECIMAL :: VK
pattern VK_DECIMAL = 0x6E
-- |
pattern VK_DIVIDE :: VK
pattern VK_DIVIDE = 0x6F

-- | 24 function keys: 'VK_F1' to 'VK_F24'
pattern VK_F1 :: VK
pattern VK_F1 = 0x70
-- |
pattern VK_F2 :: VK
pattern VK_F2 = 0x71
-- |
pattern VK_F3 :: VK
pattern VK_F3 = 0x72
-- |
pattern VK_F4 :: VK
pattern VK_F4 = 0x73
-- |
pattern VK_F5 :: VK
pattern VK_F5 = 0x74
-- |
pattern VK_F6 :: VK
pattern VK_F6 = 0x75
-- |
pattern VK_F7 :: VK
pattern VK_F7 = 0x76
-- |
pattern VK_F8 :: VK
pattern VK_F8 = 0x77
-- |
pattern VK_F9 :: VK
pattern VK_F9 = 0x78
-- |
pattern VK_F10 :: VK
pattern VK_F10 = 0x79
-- |
pattern VK_F11 :: VK
pattern VK_F11 = 0x7A
-- |
pattern VK_F12 :: VK
pattern VK_F12 = 0x7B
-- |
pattern VK_F13 :: VK
pattern VK_F13 = 0x7C
-- |
pattern VK_F14 :: VK
pattern VK_F14 = 0x7D
-- |
pattern VK_F15 :: VK
pattern VK_F15 = 0x7E
-- |
pattern VK_F16 :: VK
pattern VK_F16 = 0x7F
-- |
pattern VK_F17 :: VK
pattern VK_F17 = 0x80
-- |
pattern VK_F18 :: VK
pattern VK_F18 = 0x81
-- |
pattern VK_F19 :: VK
pattern VK_F19 = 0x82
-- |
pattern VK_F20 :: VK
pattern VK_F20 = 0x83
-- |
pattern VK_F21 :: VK
pattern VK_F21 = 0x84
-- |
pattern VK_F22 :: VK
pattern VK_F22 = 0x85
-- |
pattern VK_F23 :: VK
pattern VK_F23 = 0x86
-- |
pattern VK_F24 :: VK
pattern VK_F24 = 0x87

-- |
pattern VK_NUMLOCK :: VK
pattern VK_NUMLOCK = 0x90
-- |
pattern VK_SCROLL :: VK
pattern VK_SCROLL = 0x91

-- NEC PC-9800 kbd definitions

-- @=@ | key on numpad
pattern VK_OEM_NEC_EQUAL :: VK
pattern VK_OEM_NEC_EQUAL = 0x92

-- Fujitsu/OASYS kbd definitions

-- | @Dictionary@ key
pattern VK_OEM_FJ_JISHO :: VK
pattern VK_OEM_FJ_JISHO = 0x92
-- | @Unregister word@ key
-- |
pattern VK_OEM_FJ_MASSHOU :: VK
pattern VK_OEM_FJ_MASSHOU = 0x93
-- | @Register word@ key
-- |
pattern VK_OEM_FJ_TOUROKU :: VK
pattern VK_OEM_FJ_TOUROKU = 0x94
-- | @Left OYAYUBI@ key
-- |
pattern VK_OEM_FJ_LOYA :: VK
pattern VK_OEM_FJ_LOYA = 0x95
-- @| Right OYAYUBI@ key
-- |
pattern VK_OEM_FJ_ROYA :: VK
pattern VK_OEM_FJ_ROYA = 0x96

-- VK_L* & VK_R* - left and right Alt, Ctrl and Shift virtual keys.
-- Used only as parameters to GetAsyncKeyState() and GetKeyState().
-- No other API or message will distinguish left and right keys in this way.

-- |
pattern VK_LSHIFT :: VK
pattern VK_LSHIFT = 0xA0
-- |
pattern VK_RSHIFT :: VK
pattern VK_RSHIFT = 0xA1
-- |
pattern VK_LCONTROL :: VK
pattern VK_LCONTROL = 0xA2
-- |
pattern VK_RCONTROL :: VK
pattern VK_RCONTROL = 0xA3
-- |
pattern VK_LMENU :: VK
pattern VK_LMENU = 0xA4
-- |
pattern VK_RMENU :: VK
pattern VK_RMENU = 0xA5

-- |
pattern VK_BROWSER_BACK :: VK
pattern VK_BROWSER_BACK = 0xA6
-- |
pattern VK_BROWSER_FORWARD :: VK
pattern VK_BROWSER_FORWARD = 0xA7
-- |
pattern VK_BROWSER_REFRESH :: VK
pattern VK_BROWSER_REFRESH = 0xA8
-- |
pattern VK_BROWSER_STOP :: VK
pattern VK_BROWSER_STOP = 0xA9
-- |
pattern VK_BROWSER_SEARCH :: VK
pattern VK_BROWSER_SEARCH = 0xAA
-- |
pattern VK_BROWSER_FAVORITES :: VK
pattern VK_BROWSER_FAVORITES = 0xAB
-- |
pattern VK_BROWSER_HOME :: VK
pattern VK_BROWSER_HOME = 0xAC

-- |
pattern VK_VOLUME_MUTE :: VK
pattern VK_VOLUME_MUTE = 0xAD
-- |
pattern VK_VOLUME_DOWN :: VK
pattern VK_VOLUME_DOWN = 0xAE
-- |
pattern VK_VOLUME_UP :: VK
pattern VK_VOLUME_UP = 0xAF
-- |
pattern VK_MEDIA_NEXT_TRACK :: VK
pattern VK_MEDIA_NEXT_TRACK = 0xB0
-- |
pattern VK_MEDIA_PREV_TRACK :: VK
pattern VK_MEDIA_PREV_TRACK = 0xB1
-- |
pattern VK_MEDIA_STOP :: VK
pattern VK_MEDIA_STOP = 0xB2
-- |
pattern VK_MEDIA_PLAY_PAUSE :: VK
pattern VK_MEDIA_PLAY_PAUSE = 0xB3
-- |
pattern VK_LAUNCH_MAIL :: VK
pattern VK_LAUNCH_MAIL = 0xB4
-- |
pattern VK_LAUNCH_MEDIA_SELECT :: VK
pattern VK_LAUNCH_MEDIA_SELECT = 0xB5
-- |
pattern VK_LAUNCH_APP1 :: VK
pattern VK_LAUNCH_APP1 = 0xB6
-- |
pattern VK_LAUNCH_APP2 :: VK
pattern VK_LAUNCH_APP2 = 0xB7

-- | @;:@ for the US
-- |
pattern VK_OEM_1 :: VK
pattern VK_OEM_1 = 0xBA
-- | @+@ any country
-- |
pattern VK_OEM_PLUS :: VK
pattern VK_OEM_PLUS = 0xBB
-- | @,@ | any country
-- |
pattern VK_OEM_COMMA :: VK
pattern VK_OEM_COMMA = 0xBC
-- | @-@ | any country
-- |
pattern VK_OEM_MINUS :: VK
pattern VK_OEM_MINUS = 0xBD
-- | @.@ | any country
-- |
pattern VK_OEM_PERIOD :: VK
pattern VK_OEM_PERIOD = 0xBE
-- | @/?@ | for the US
-- |
pattern VK_OEM_2 :: VK
pattern VK_OEM_2 = 0xBF
-- | @`~@ | for the US
-- |
pattern VK_OEM_3 :: VK
pattern VK_OEM_3 = 0xC0

-- | @[{@ | for the US
-- |
pattern VK_OEM_4 :: VK
pattern VK_OEM_4 = 0xDB
-- |  @\|@ | for the US
-- |
pattern VK_OEM_5 :: VK
pattern VK_OEM_5 = 0xDC
-- |  @]}@ | for the US
-- |
pattern VK_OEM_6 :: VK
pattern VK_OEM_6 = 0xDD
-- | @"@ for the US
-- |
pattern VK_OEM_7 :: VK
pattern VK_OEM_7 = 0xDE
-- |
pattern VK_OEM_8 :: VK
pattern VK_OEM_8 = 0xDF

-- Various extended or enhanced keyboards

-- | AX key on Japanese AX kbd
--
pattern VK_OEM_AX :: VK
pattern VK_OEM_AX = 0xE1
-- | @"<>"@ or @"\|"@ on RT 102-key kbd.
--
pattern VK_OEM_102 :: VK
pattern VK_OEM_102 = 0xE2
-- | "Help key on ICO"
pattern VK_ICO_HELP :: VK
pattern VK_ICO_HELP = 0xE3
-- | "00 key on ICO"
--
pattern VK_ICO_00 :: VK
pattern VK_ICO_00 = 0xE4

-- |
pattern VK_PROCESSKEY :: VK
pattern VK_PROCESSKEY = 0xE5

-- |
pattern VK_ICO_CLEAR :: VK
pattern VK_ICO_CLEAR = 0xE6

-- | "Windows 2000: Used to pass Unicode characters as if they were keystrokes. The VK_PACKET key is the low word of a 32-bit Virtual Key value used for non-keyboard input methods. The Unicode character is the high word."
pattern VK_PACKET :: VK
pattern VK_PACKET = 0xE7

-- |
pattern VK_OEM_RESET :: VK
pattern VK_OEM_RESET = 0xE9
-- |
pattern VK_OEM_JUMP :: VK
pattern VK_OEM_JUMP = 0xEA
-- |
pattern VK_OEM_PA1 :: VK
pattern VK_OEM_PA1 = 0xEB
-- |
pattern VK_OEM_PA2 :: VK
pattern VK_OEM_PA2 = 0xEC
-- |
pattern VK_OEM_PA3 :: VK
pattern VK_OEM_PA3 = 0xED
-- |
pattern VK_OEM_WSCTRL :: VK
pattern VK_OEM_WSCTRL = 0xEE
-- |
pattern VK_OEM_CUSEL :: VK
pattern VK_OEM_CUSEL = 0xEF
-- |
pattern VK_OEM_ATTN :: VK
pattern VK_OEM_ATTN = 0xF0
-- |
pattern VK_OEM_FINISH :: VK
pattern VK_OEM_FINISH = 0xF1
-- |
pattern VK_OEM_COPY :: VK
pattern VK_OEM_COPY = 0xF2
-- |
pattern VK_OEM_AUTO :: VK
pattern VK_OEM_AUTO = 0xF3
-- |
pattern VK_OEM_ENLW :: VK
pattern VK_OEM_ENLW = 0xF4
-- |
pattern VK_OEM_BACKTAB :: VK
pattern VK_OEM_BACKTAB = 0xF5

-- |
pattern VK_ATTN :: VK
pattern VK_ATTN = 0xF6
-- |
pattern VK_CRSEL :: VK
pattern VK_CRSEL = 0xF7
-- |
pattern VK_EXSEL :: VK
pattern VK_EXSEL = 0xF8
-- |
pattern VK_EREOF :: VK
pattern VK_EREOF = 0xF9
-- |
pattern VK_PLAY :: VK
pattern VK_PLAY = 0xFA
-- |
pattern VK_ZOOM :: VK
pattern VK_ZOOM = 0xFB
-- |
pattern VK_NONAME :: VK
pattern VK_NONAME = 0xFC
-- |
pattern VK_PA1 :: VK
pattern VK_PA1 = 0xFD
-- |
pattern VK_OEM_CLEAR :: VK
pattern VK_OEM_CLEAR = 0xFE

-- | @Fn@ key "on most laptops" (undocumented)
--
-- http://stackoverflow.com/questions/4718069/what-is-win32-virtual-key-code-0xff-used-for-and-is-it-documented-somewhere
pattern VK_FN :: VK
pattern VK_FN = 0xFF
