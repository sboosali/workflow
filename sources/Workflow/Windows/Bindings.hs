{-# LANGUAGE ForeignFunctionInterface, CPP, ViewPatterns, RecordWildCards #-}
{-|

all derived in Haskell

-}
module Workflow.Windows.Bindings where
import Workflow.Windows.Types
import Workflow.Windows.Extra

import Foreign.C
import Data.Char
import Data.Foldable
import Numeric.Natural

#include "calling_convention.h"

{-
::  -> IO ()
 = c_

foreign import CALLING_CONVENTION unsafe "Workflow.h "
 c_ ::  -> IO ()
-}

--------------------------------------------------------------------------------

getClipboard :: IO String
getClipboard = c_GetClipboard >>= peekCWString

foreign import CALLING_CONVENTION unsafe "Workflow.h GetClipboard"
 c_GetClipboard :: IO CWString

setClipboard :: String -> IO ()
setClipboard s = withCWString s c_SetClipboard

foreign import CALLING_CONVENTION unsafe "Workflow.h SetClipboard"
 c_SetClipboard :: CWString -> IO ()

--------------------------------------------------------------------------------

{-| inserts some text into the current application.

char-by-char (one per event), no delay (between events).

-}
sendText :: String -> IO ()
sendText = traverse_ sendChar -- TODO delay?

{-|

milliseconds

-}
sendTextDelaying :: Int -> String -> IO ()
sendTextDelaying i = traverse_ (\c -> sendChar c >> delay i)

sendChar :: Char -> IO ()
sendChar c = do
 _ <- c_SendUnicodeChar ((CWchar . fromIntegral . ord) c) -- cast doesn't overflow, there are ~1,000,000 chars.
 return ()

foreign import CALLING_CONVENTION unsafe "Workflow.h SendUnicodeChar"
 c_SendUnicodeChar :: WCHAR_T -> IO UINT

--------------------------------------------------------------------------------

pressKeychord :: [VK] -> VK -> IO ()
pressKeychord modifiers key = do
  pressKeyDown `traverse_` modifiers
  pressKeyDown key
  pressKeyUp   key
  pressKeyUp   `traverse_` (reverse modifiers)

pressKey :: VK -> IO ()
pressKey key = do
 pressKeyDown key
 pressKeyUp   key

{-|

milliseconds

-}
pressKeyDelaying :: Int -> VK -> IO ()
pressKeyDelaying milliseconds key = do
 pressKeyDown key
 delay milliseconds -- TODO is threadDelay 0 like noop?
 pressKeyUp   key

pressKeyDown :: VK -> IO ()
pressKeyDown = c_PressKeyDown . getVK

foreign import CALLING_CONVENTION unsafe "Workflow.h PressKeyDown"
 c_PressKeyDown :: WORD -> IO ()

pressKeyUp :: VK -> IO ()
pressKeyUp = c_PressKeyUp . getVK

foreign import CALLING_CONVENTION unsafe "Workflow.h PressKeyUp"
 c_PressKeyUp :: WORD -> IO ()

--------------------------------------------------------------------------------

--clickMouse :: Natural -> IO ()

--moveMouseTo ::
--moveMouseTo =

clickMouseAt :: POINT -> Natural -> MOUSEEVENTF -> MOUSEEVENTF -> IO ()
clickMouseAt POINT{..} times down up
 = c_ClickMouseAt (toInt _x) (toInt _y) (toInt times) (getMOUSEEVENTF down) (getMOUSEEVENTF up)

foreign import CALLING_CONVENTION unsafe "Workflow.h ClickMouseAt"
  c_ClickMouseAt :: Int -> Int -> Int -> DWORD -> DWORD -> IO ()

hs_ScrollMouseWheel :: MouseWheel -> Direction -> Natural -> IO () --TODO reversed? or my trackpad settings?
hs_ScrollMouseWheel wheel direction distance = c_ScrollMouseWheel
 (getMOUSEEVENTF . encodeMouseWheel $ wheel)
 (encodeDirection direction)
 (toDWORD distance)

foreign import CALLING_CONVENTION unsafe "Workflow.h ScrollMouseWheel"
 c_ScrollMouseWheel :: DWORD -> DWORD -> DWORD -> IO ()

--GetCursorPos

-- MOUSEEVENTF_MOVE .|. MOUSEEVENTF_ABSOLUTE .|. buttonDown .|. buttonUp
-- foldl (.|.) [MOUSEEVENTF_MOVE, MOUSEEVENTF_ABSOLUTE, buttonDown, buttonUp]

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

encodeMouseWheel :: MouseWheel -> MOUSEEVENTF
encodeMouseWheel = \case
 VerticalWheel   -> MOUSEEVENTF_WHEEL
 HorizontalWheel -> MOUSEEVENTF_HWHEEL

encodeDirection :: Direction -> DWORD
encodeDirection = \case
 Forwards  -> 1 -- right?
 Backwards -> -1 -- left?

encodeMouseButton :: MouseButton -> (MOUSEEVENTF,MOUSEEVENTF)
encodeMouseButton = \case
 LeftButton   -> (MOUSEEVENTF_LEFTDOWN   , MOUSEEVENTF_LEFTUP)
 MiddleButton -> (MOUSEEVENTF_MIDDLEDOWN , MOUSEEVENTF_MIDDLEUP)
 RightButton  -> (MOUSEEVENTF_RIGHTDOWN  , MOUSEEVENTF_RIGHTUP)
 XButton      -> (MOUSEEVENTF_XDOWN      , MOUSEEVENTF_XUP)
