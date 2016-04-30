{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Workflow.Windows.Types where

import Foreign.C.Types
import Foreign.C.String
import Data.Word
import Data.Int
import GHC.Exts

type LPCWSTR = CWString

type WCHAR_T = CWchar

type UINT = Word32 -- CUInt

type WORD = Word16

type DWORD = Word32

type LONG = Int64 -- CLong

-- void* (in both Haskell and C)
newtype HWND = HWND (Ptr ())
-- type HWND = Ptr ()

{-| Keyboard/Mouse as union of like all possible virtual keyboards/mice?
that's just an int.
or intersection, as the point of Workflow is cross-platform actions.

-}

{-|

takes an enum to a virtual key code.

-}
data Keyboard key = Keyboard
 { fromKey      :: key      -> WORD -- CULong
 -- , fromModifier :: modifier -> WORD
 }
-- deriving (Contravariant)

{-|

@
i :: Injections a b
(i&forwardInjection) >=> (i&backwardInjection) === (id ||| const Nothing)
(i&forwardInjection) <=< (i&backwardInjection) === (id ||| const Nothing)
@

@

roundtripForwards :: Injections a b -> (a -> Maybe a)
roundtripForwards  i = (i&forwardInjection) >=> (i&backwardInjection)

roundtripBackwards :: Injections a b -> (b -> Maybe b)
roundtripBackwards i = (i&forwardInjection) <=< (i&backwardInjection)

case roundtripForwards i x of
 Nothing -> True
 Just y  -> x == y

law> roundtripForwards  i a >>> maybe True (== a)
law> roundtripBackwards i b >>> maybe True (== b)

NO

\forall a -> case (i&forwardInjection) a of
 Nothing -> \forall b -> (i&backwardInjection) b \= Just a
 Just b -> (i&backwardInjection) b == a

-}
data Injections a b = Injections --TODO
 { forwardInjection  :: a -> Maybe b
 , backwardInjection :: b -> Maybe a
 }
-- instance Profunctor Injections

{-|

takes an enum to a virtual code.

"gizmo"s include:

* mouse buttons
* mouse wheels

-}
data Mouse gizmo = Mouse
 { fromGizmo :: gizmo -> DWORD -- CULong
 }
 -- deriving (Contravariant)

newtype Application = Application String
 deriving (Show, IsString)

newtype URL = URL String
 deriving (Show, IsString)


{-|

see <https://msdn.microsoft.com/en-us/library/windows/desktop/dd375731(v=vs.85).aspx Virtual-Key Codes>

-}
data VK

   = VK_BACK           -- ^
   | VK_TAB            -- ^

   | VK_CLEAR          -- ^
   | VK_RETURN         -- ^
   | VK_SHIFT          -- ^ SHIFT
   | VK_CONTROL        -- ^ CTRL

   | VK_MENU           -- ^ ALT
   | VK_PAUSE          -- ^
   | VK_CAPITAL        -- ^

   | VK_KANA           -- ^
   | VK_HANGEUL        -- ^
   | VK_HANGUL         -- ^
   | VK_JUNJA          -- ^
   | VK_FINAL          -- ^
   | VK_HANJA          -- ^
   | VK_KANJI          -- ^
   | VK_ESCAPE         -- ^

   | VK_CONVERT        -- ^
   | VK_NONCONVERT     -- ^
   | VK_ACCEPT         -- ^
   | VK_MODECHANGE     -- ^

   | VK_SPACE          -- ^
   | VK_PRIOR          -- ^
   | VK_NEXT           -- ^
   | VK_END            -- ^
   | VK_HOME           -- ^
   | VK_LEFT           -- ^
   | VK_UP             -- ^
   | VK_RIGHT          -- ^
   | VK_DOWN           -- ^
   | VK_SELECT         -- ^
   | VK_PRINT          -- ^
   | VK_EXECUTE        -- ^
   | VK_SNAPSHOT       -- ^
   | VK_INSERT         -- ^
   | VK_DELETE         -- ^
   | VK_HELP           -- ^

   | VK_0              -- ^
   | VK_1              -- ^
   | VK_2              -- ^
   | VK_3              -- ^
   | VK_4              -- ^
   | VK_5              -- ^
   | VK_6              -- ^
   | VK_7              -- ^
   | VK_8              -- ^
   | VK_9              -- ^

   | VK_A              -- ^
   | VK_B              -- ^
   | VK_C              -- ^
   | VK_D              -- ^
   | VK_E              -- ^
   | VK_F              -- ^
   | VK_G              -- ^
   | VK_H              -- ^
   | VK_I              -- ^
   | VK_J              -- ^
   | VK_K              -- ^
   | VK_L              -- ^
   | VK_M              -- ^
   | VK_N              -- ^
   | VK_O              -- ^
   | VK_P              -- ^
   | VK_Q              -- ^
   | VK_R              -- ^
   | VK_S              -- ^
   | VK_T              -- ^
   | VK_U              -- ^
   | VK_V              -- ^
   | VK_W              -- ^
   | VK_X              -- ^
   | VK_Y              -- ^
   | VK_Z              -- ^

   | VK_LWIN           -- ^ WINDOWS
   | VK_RWIN           -- ^
   | VK_APPS           -- ^

   | VK_SLEEP          -- ^

   | VK_NUMPAD0        -- ^
   | VK_NUMPAD1        -- ^
   | VK_NUMPAD2        -- ^
   | VK_NUMPAD3        -- ^
   | VK_NUMPAD4        -- ^
   | VK_NUMPAD5        -- ^
   | VK_NUMPAD6        -- ^
   | VK_NUMPAD7        -- ^
   | VK_NUMPAD8        -- ^
   | VK_NUMPAD9        -- ^
   | VK_MULTIPLY       -- ^
   | VK_ADD            -- ^
   | VK_SEPARATOR      -- ^
   | VK_SUBTRACT       -- ^
   | VK_DECIMAL        -- ^
   | VK_DIVIDE         -- ^
   | VK_F1             -- ^
   | VK_F2             -- ^
   | VK_F3             -- ^
   | VK_F4             -- ^
   | VK_F5             -- ^
   | VK_F6             -- ^
   | VK_F7             -- ^
   | VK_F8             -- ^
   | VK_F9             -- ^
   | VK_F10            -- ^
   | VK_F11            -- ^
   | VK_F12            -- ^
   | VK_F13            -- ^
   | VK_F14            -- ^
   | VK_F15            -- ^
   | VK_F16            -- ^
   | VK_F17            -- ^
   | VK_F18            -- ^
   | VK_F19            -- ^
   | VK_F20            -- ^
   | VK_F21            -- ^
   | VK_F22            -- ^
   | VK_F23            -- ^
   | VK_F24            -- ^

   | VK_NUMLOCK        -- ^
   | VK_SCROLL         -- ^

   | VK_OEM_NEC_EQUAL  -- ^

   | VK_OEM_FJ_JISHO   -- ^
   | VK_OEM_FJ_MASSHOU -- ^
   | VK_OEM_FJ_TOUROKU -- ^
   | VK_OEM_FJ_LOYA    -- ^
   | VK_OEM_FJ_ROYA    -- ^

   | VK_LSHIFT         -- ^
   | VK_RSHIFT         -- ^
   | VK_LCONTROL       -- ^
   | VK_RCONTROL       -- ^
   | VK_LMENU          -- ^
   | VK_RMENU          -- ^

   | VK_BROWSER_BACK        -- ^
   | VK_BROWSER_FORWARD     -- ^
   | VK_BROWSER_REFRESH     -- ^
   | VK_BROWSER_STOP        -- ^
   | VK_BROWSER_SEARCH      -- ^
   | VK_BROWSER_FAVORITES   -- ^
   | VK_BROWSER_HOME        -- ^

   | VK_VOLUME_MUTE         -- ^
   | VK_VOLUME_DOWN         -- ^
   | VK_VOLUME_UP           -- ^
   | VK_MEDIA_NEXT_TRACK    -- ^
   | VK_MEDIA_PREV_TRACK    -- ^
   | VK_MEDIA_STOP          -- ^
   | VK_MEDIA_PLAY_PAUSE    -- ^
   | VK_LAUNCH_MAIL         -- ^
   | VK_LAUNCH_MEDIA_SELECT -- ^
   | VK_LAUNCH_APP1         -- ^
   | VK_LAUNCH_APP2         -- ^

   | VK_OEM_1          -- ^
   | VK_OEM_PLUS       -- ^
   | VK_OEM_COMMA      -- ^
   | VK_OEM_MINUS      -- ^
   | VK_OEM_PERIOD     -- ^
   | VK_OEM_2          -- ^
   | VK_OEM_3          -- ^

   | VK_OEM_4          -- ^
   | VK_OEM_5          -- ^
   | VK_OEM_6          -- ^
   | VK_OEM_7          -- ^
   | VK_OEM_8          -- ^

   | VK_OEM_AX         -- ^
   | VK_OEM_102        -- ^
   | VK_ICO_HELP       -- ^
   | VK_ICO_00         -- ^

   | VK_PROCESSKEY     -- ^

   | VK_ICO_CLEAR      -- ^

   | VK_PACKET         -- ^

   | VK_OEM_RESET      -- ^
   | VK_OEM_JUMP       -- ^
   | VK_OEM_PA1        -- ^
   | VK_OEM_PA2        -- ^
   | VK_OEM_PA3        -- ^
   | VK_OEM_WSCTRL     -- ^
   | VK_OEM_CUSEL      -- ^
   | VK_OEM_ATTN       -- ^
   | VK_OEM_FINISH     -- ^
   | VK_OEM_COPY       -- ^
   | VK_OEM_AUTO       -- ^
   | VK_OEM_ENLW       -- ^
   | VK_OEM_BACKTAB    -- ^

   | VK_ATTN           -- ^
   | VK_CRSEL          -- ^
   | VK_EXSEL          -- ^
   | VK_EREOF          -- ^
   | VK_PLAY           -- ^
   | VK_ZOOM           -- ^
   | VK_NONAME         -- ^
   | VK_PA1            -- ^
   | VK_OEM_CLEAR      -- ^

   deriving (Show,Enum)

data MouseButton
 = LeftButton
 | MiddleButton
 | RightButton
 | XButton
 deriving (Show,Enum)

data MouseWheel
 = VerticalWheel
 | HorizontalWheel
 deriving (Show,Enum)

data MOUSEEVENTF
 = MOUSEEVENTF_MOVE        -- ^ mouse move
 | MOUSEEVENTF_LEFTDOWN    -- ^ left button down
 | MOUSEEVENTF_LEFTUP      -- ^ left button up
 | MOUSEEVENTF_RIGHTDOWN   -- ^ right button down
 | MOUSEEVENTF_RIGHTUP     -- ^ right button up
 | MOUSEEVENTF_MIDDLEDOWN  -- ^ middle button down
 | MOUSEEVENTF_MIDDLEUP    -- ^ middle button up
 | MOUSEEVENTF_XDOWN       -- ^ x button down
 | MOUSEEVENTF_XUP         -- ^ x button down
 | MOUSEEVENTF_WHEEL                -- ^ wheel button rolled
 | MOUSEEVENTF_HWHEEL               -- ^ hwheel button rolled
 | MOUSEEVENTF_MOVE_NOCOALESCE      -- ^ do not coalesce mouse moves
 | MOUSEEVENTF_VIRTUALDESK          -- ^ map to entire virtual desktop
 | MOUSEEVENTF_ABSOLUTE             -- ^ absolute move
 deriving (Show,Enum)

data Point = Point
 { _x :: LONG
 , _y :: LONG
 }

{-|

@
struct POINT
{
    LONG  x;
    LONG  y;
}
@

instance Storeable Point where
  sizeOf _  = sizeOf (0::LONG) + sizeOf (0::LONG)

  -- "The entire structure is aligned on a boundary
  -- at least as big as the biggest value in the structure"
  alignment = sizeOf (0::LONG)

  -- peekByteOff address offset = peek (address `plusPtr` offset)
  peek p = do
    _x <- peekByteOff p (0 * sizeOf (0::LONG))
    _y <- peekByteOff p (4 * sizeOf (0::LONG))
    return Point{..}

  -- pokeByteOff addr off x = poke (addr `plusPtr` off) x
  poke p Point{..} = do
    pokeByteOff p (0 * sizeOf (0::LONG)) _x
    pokeByteOff p (4 * sizeOf (0::LONG)) _y
    -}
