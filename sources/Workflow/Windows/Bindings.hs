{-# LANGUAGE ViewPatterns, RecordWildCards #-}
{-|

all derived in Haskell

-}
module Workflow.Windows.Bindings where
import Workflow.Windows.Types
import Workflow.Windows.Extra
import Workflow.Windows.Foreign
import Workflow.Windows.Constants

import Foreign
import Foreign.C
-- import Foreign.C.String
import Data.Char
import Numeric.Natural
import Control.Monad.IO.Class
import Control.Exception (bracket)


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

-- data MouseMotion = MoveMouseRelatively Interval | MoveMouseAbsolutely POINT
--moveMouse :: (MonadIO m) => MouseMotion -> m ()
--moveMouse =

-- absolute coordinates
-- getMousePosition :: (MonadIO m) => m POINT

-- getScreenSize :: (MonadIO m) => m RECT
-- getClientSize :: (MonadIO m) => m RECT

clickMouseAt :: (MonadIO m) => POINT -> Natural -> MOUSEEVENTF -> MOUSEEVENTF -> m ()
clickMouseAt (POINT x y) times down up = liftIO $
 c_ClickMouseAt (toInt x) (toInt y) (toInt times) (getMOUSEEVENTF down) (getMOUSEEVENTF up)

-- type IO' a = (MonadIO m) => m a

{-|

distance: 120 units is about one "tick" of the wheel
(i.e. a few dozen nudges the screen).

-}
scrollMouse :: (MonadIO m) => MouseScroll -> Natural -> m () --TODO reversed? or my trackpad settings?
scrollMouse (encodeMouseScroll -> (wheel, direction)) distance = liftIO $ c_ScrollMouseWheel
 (wheel & getMOUSEEVENTF)
 (direction)
 (distance & toDWORD)

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

{-|

-}
getCursorPosition :: (MonadIO m) => m POINT
getCursorPosition = liftIO $ getByReference c_GetCursorPos
{-|

(doesn't trigger @RSIGuard@'s @AutoClick@).

-}
setCursorPosition :: (MonadIO m) => POINT -> m ()
setCursorPosition (POINT x y) = liftIO $ do
  c_SetCursorPos (CInt x) (CInt y)

-------------------------------------------------------------------------

findWindow :: (MonadIO m) => Window -> m HWND
findWindow Window{..} = liftIO $ do
  withCWString windowClass $ \_windowClass -> do -- ContT IO?
    withCWString windowTitle $ \_windowTitle -> do
        HWND <$> c_FindWindow _windowClass _windowTitle --TODO _windowExecutable

-- | TODO check against 'nullPtr'?
getWindowRectangle :: (MonadIO m) => HWND -> m RECT
getWindowRectangle (HWND w) = liftIO $ getByReference (c_GetWindowRect w)

getLastError :: (MonadIO m) => m SystemErrorCode
getLastError = liftIO $ SystemErrorCode <$> c_GetLastError

-------------------------------------------------------------------------

{- for reference-parameter "getters" (unary).

-}
getByReference :: (Storable a) => (Ptr a -> IO ()) -> IO a
getByReference setter = bracket
 malloc
 free
 (\p -> setter p >> peek p)

 -- getByReference :: (Storable a, MonadIO m) => (Ptr a -> m ()) -> m a
 -- getByReference setter = liftIO $ bracket -- can liftIO? result IO is positive-position, but action IO is negative-position.
 --  malloc
 --  free
 --  (\p -> setter p >> liftIO (peek p))

{-|

@
(wheel, direction) = encodeMouseScroll
@

-}
encodeMouseScroll :: MouseScroll -> (MOUSEEVENTF, DWORD)
encodeMouseScroll = \case
    ScrollTowards -> (MOUSEEVENTF_WHEEL,   1)
    ScrollAway    -> (MOUSEEVENTF_WHEEL,  -1)
    ScrollRight   -> (MOUSEEVENTF_HWHEEL,  1)
    ScrollLeft    -> (MOUSEEVENTF_HWHEEL, -1)

{-|

@
(downEvent, upEvent) = encodeMouseButton
@

-}
encodeMouseButton :: MouseButton -> (MOUSEEVENTF,MOUSEEVENTF)
encodeMouseButton = \case
 LeftButton   -> (MOUSEEVENTF_LEFTDOWN   , MOUSEEVENTF_LEFTUP)
 MiddleButton -> (MOUSEEVENTF_MIDDLEDOWN , MOUSEEVENTF_MIDDLEUP)
 RightButton  -> (MOUSEEVENTF_RIGHTDOWN  , MOUSEEVENTF_RIGHTUP)
 XButton      -> (MOUSEEVENTF_XDOWN      , MOUSEEVENTF_XUP)
