{-# LANGUAGE ViewPatterns, RecordWildCards #-}
{-|

all derived in Haskell

-}
module Workflow.Windows.Bindings where
import Workflow.Windows.Types
import Workflow.Windows.Extra
import Workflow.Windows.Foreign

import Foreign.C
import Data.Char
import Numeric.Natural
import Control.Monad.IO.Class

{-
::  -> IO ()
 = c_

foreign import CALLING_CONVENTmN unsafe "Workflow.h "
 c_ ::  -> IO ()
-}

--------------------------------------------------------------------------------

getClipboard :: (MonadIO m) => m String
getClipboard = liftIO $ c_GetClipboard >>= peekCWString

setClipboard :: (MonadIO m) => String -> m ()
setClipboard s = liftIO $ withCWString s c_SetClipboard

--------------------------------------------------------------------------------

{-| inserts some text into the current Application.

char-by-char (one per event), no delay (between events).

-}
sendText :: (MonadIO m) => String -> m ()
sendText = traverse_ sendChar -- TODO delay?

{-|

milliseconds

-}
sendTextDelaying :: (MonadIO m) => Int -> String -> m ()
sendTextDelaying i = traverse_ (\c -> sendChar c >> delayMilliseconds i)

sendChar :: (MonadIO m) => Char -> m ()
sendChar c = liftIO $ do
 _ <- c_SendUnicodeChar ((CWchar . fromIntegral . ord) c) -- cast doesn't overflow, there are ~1,000,000 chars.
 return ()

--------------------------------------------------------------------------------

pressKeychord :: (MonadIO m) => [VK] -> VK -> m ()
pressKeychord modifiers key = do
  pressKeyDown `traverse_` modifiers
  pressKeyDown key
  pressKeyUp   key
  pressKeyUp   `traverse_` (reverse modifiers)

pressKey :: (MonadIO m) => VK -> m ()
pressKey key = do
 pressKeyDown key
 pressKeyUp   key

{-|

milliseconds

-}
pressKeyDelaying :: (MonadIO m) => Int -> VK -> m ()
pressKeyDelaying t key = do
 pressKeyDown key
 delayMilliseconds t -- TODO is threadDelay 0 like noop?
 pressKeyUp   key

pressKeyDown :: (MonadIO m) => VK -> m ()
pressKeyDown = liftIO . c_PressKeyDown . getVK

pressKeyUp :: (MonadIO m) => VK -> m ()
pressKeyUp = liftIO . c_PressKeyUp . getVK

--------------------------------------------------------------------------------

--clickMouse :: Natural -> m ()

--moveMouseTo ::
--moveMouseTo =

clickMouseAt :: (MonadIO m) => POINT -> Natural -> MOUSEEVENTF -> MOUSEEVENTF -> m ()
clickMouseAt POINT{..} times down up
 = liftIO $ c_ClickMouseAt (toInt _x) (toInt _y) (toInt times) (getMOUSEEVENTF down) (getMOUSEEVENTF up)

hs_ScrollMouseWheel :: (MonadIO m) => MouseWheel -> Direction -> Natural -> m () --TODO reversed? or my trackpad settings?
hs_ScrollMouseWheel wheel direction distance = liftIO $ c_ScrollMouseWheel
 (wheel & encodeMouseWheel & getMOUSEEVENTF)
 (encodeDirection direction)
 (toDWORD distance)

--GetCursorPos

-- MOUSEEVENTF_MOVE .|. MOUSEEVENTF_ABSOLUTE .|. buttonDown .|. buttonUp
-- foldl (.|.) [MOUSEEVENTF_MOVE, MOUSEEVENTF_ABSOLUTE, buttonDown, buttonUp]

--------------------------------------------------------------------------------

currentApplication :: (MonadIO m) => m Application
currentApplication = Application <$> todo

{-|
TODO windows "apps"
launchApplication :: String -> m ()
launchApplication s = withCWString s c_LaunchApplication
-}

openApplication :: (MonadIO m) => Application -> m () -- launchApplication?
openApplication (Application s) = liftIO $ withCWString s c_OpenApplication

openUrl :: (MonadIO m) => URL -> m () -- visitURL?
openUrl (URL s) = liftIO $ withCWString s c_OpenUrl

------------------------------------------------------------------------

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
