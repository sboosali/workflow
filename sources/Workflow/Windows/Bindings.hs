{-# LANGUAGE ViewPatterns, RecordWildCards #-}
{-|

medium-level bindings.

(all derived in Haskell. TODO wat?)

-}
module Workflow.Windows.Bindings where
import Workflow.Windows.Types
import Workflow.Windows.Extra
import Workflow.Windows.Foreign
-- import Workflow.Windows.Constants

import Foreign
import Foreign.C
-- import Foreign.C.String
import Data.Char
import Numeric.Natural
import Control.Monad.IO.Class
import Control.Exception (bracket,bracket_)

import Prelude.Spiros (todo,delayMilliseconds)

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

-- @= 'sendText_byChar'@
sendText :: (MonadIO m) => String -> m ()
sendText = sendText_byChar

{-| inserts some text into the current Application.

char-by-char (one per event), no delay (between events).

@
= 'traverse_' 'sendChar'
@

-}
sendText_byChar :: (MonadIO m) => String -> m ()
sendText_byChar = traverse_ sendChar

{-| like 'sendText', interspersing a delay (in milliseconds)

-}
sendTextDelaying :: (MonadIO m) => Int -> String -> m ()
sendTextDelaying i = traverse_ (\c -> sendChar c >> delayMilliseconds i)

sendChar :: (MonadIO m) => Char -> m ()
sendChar c = liftIO $ do
 _ <- c_SendUnicodeChar ((CWchar . fromIntegral . ord) c) -- cast doesn't overflow, there are ~1,000,000 chars.
 return ()

--------------------------------------------------------------------------------

--TODO workflow-types holdingModifiers

{- perform an action while holding down some keys (e.g. modifiers).

via 'bracket_', the keys are released even when an exception is raised.

-}
holdingKeys :: [VK] -> IO () -> IO ()
holdingKeys keys = bracket_
 (pressKeyDown `traverse_` keys)
 (pressKeyUp   `traverse_` keys)

-- holdingKeys :: (MonadIO m) => [VK] -> m () -> m ()
-- holdingKeys keys action = liftIO $ bracket_
--  (pressKeyDown `traverse_` keys)
--  (pressKeyUp   `traverse_` keys)
--  action

pressKeyChord :: (MonadIO m) => [VK] -> VK -> m () --TODO rn KeyChord
pressKeyChord modifiers key = liftIO $ do
  holdingKeys modifiers $ do
      pressKeyDown key
      pressKeyUp   key

pressKey :: (MonadIO m) => VK -> m ()
pressKey key = liftIO $ do
 pressKeyDown key
 pressKeyUp   key

{-|

milliseconds

-}
pressKeyDelaying :: (MonadIO m) => Int -> VK -> m ()
pressKeyDelaying t key = do
 liftIO $ pressKeyDown key
 delayMilliseconds t -- TODO is threadDelay 0 like noop?
 liftIO $ pressKeyUp   key

pressKeyDown :: VK -> IO ()
pressKeyDown = c_PressKeyDown . getVK

pressKeyUp :: VK -> IO ()
pressKeyUp = c_PressKeyUp . getVK

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

input:

* 'True' is towards
* 'False' is away

-}
scrollMouse :: (MonadIO m) => MOUSEEVENTF -> DWORD -> Natural -> m ()
scrollMouse wheel direction distance = liftIO $ c_ScrollMouseWheel
 (wheel & getMOUSEEVENTF)
 direction -- (if direction then 1 else -1) -- seems to work, even though Word's are unsigned.
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
