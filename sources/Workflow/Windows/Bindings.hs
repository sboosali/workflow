{-# LANGUAGE ViewPatterns, RecordWildCards,  ScopedTypeVariables  #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
{-|

medium-level bindings.

(all derived in Haskell. TODO wat?)

TODO problem : adjacent duplicate characters are *always* droped

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
import Control.Exception (bracket,bracket_)
import qualified Language.C.Inline as C

import Prelude (error)

C.include "<stdio.h>"
C.include "<math.h>"

testInlineC :: IO ()
testInlineC = do
   x <- [C.exp| int{ printf("Some number: %.2f\n", cos(0.5)) } |]
   putStrLn $ show x ++ " characters printed."

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

{-| like 'sendText', 'intersperse'-ing a delay (in milliseconds)

-}
sendTextDelaying_byChar :: (MonadIO m) => Int -> String -> m ()
sendTextDelaying_byChar i = fmap sendChar > _interspersed i > sequence_
  where
  _interspersed = \case
    0 -> id
    j -> intersperse (delayMilliseconds j)
  -- traverse_ (\c -> sendChar c >> delayMilliseconds i)
  -- intersperse

-------------------------------------------------------------------------------

{-| a syntax tree that represents the insertion of the particular characters in some string.

-}
data CharacterInsertion = InsertCharacter Char | DelayMilliseconds Int
  deriving (Show,Eq)

{-| like 'sendText', 'intersperse'-ing two delays (in milliseconds): the second between
adjacent duplicate characters, and the first between any other character pair.

if the string has no adjacent duplicate characters, it's equivalent to 'sendTextDelaying_byChar'

i.e. if @length (group s) == length s@, then
@'sendTextDelayingDuplicates_byChar' i j s == 'sendTextDelaying_byChar' i s@

e.g

>>> group "hello"
["h","e","ll","o"]

-}
sendTextByCharacterDelayingAdjacentDuplicates :: (MonadIO m) => Int -> Int -> String -> m ()
sendTextByCharacterDelayingAdjacentDuplicates defaultDelay duplicateDelay
  = insertCharactersDelayingAdjacentDuplicates defaultDelay duplicateDelay
  > evaluateCharacterInsertions
{-# SPECIALIZE sendTextByCharacterDelayingAdjacentDuplicates :: Int -> Int -> String -> IO () #-}

-- sendTextDelayingDuplicates_byChar :: (MonadIO m) => Int -> Int -> String -> m ()
-- sendTextDelayingDuplicates_byChar defaultDelay duplicateDelay =
--   group > fmap (fmap sendChar > _delayed duplicateDelay > sequence_) > _delayed defaultDelay > sequence_
--   where
--   _delayed :: (MonadIO m) => Int -> [m ()] -> [m ()]
--   _delayed = \case
--     0 -> id
--     j -> intersperse (delayMilliseconds j)
-- {-# SPECIALIZE sendTextDelayingDuplicates_byChar :: Int -> Int -> String -> IO () #-}

{-| build a syntax tree that represents the insertion of the particular characters in some string.

e.g

>>> group "hello"
["h","e","ll","o"]

>>> insertCharactersDelayingAdjacentDuplicates 0 30 "hello"
[InsertCharacter 'h',InsertCharacter 'e',InsertCharacter 'l', DelayMilliseconds 30,InsertCharacter 'l',InsertCharacter 'o']

>>> insertCharactersDelayingAdjacentDuplicates 1 30 "hello"
[InsertCharacter 'h',DelayMilliseconds 1,InsertCharacter 'e',DelayMilliseconds 1,InsertCharacter 'l',DelayMilliseconds 30,InsertCharacter 'l',DelayMilliseconds 1,InsertCharacter 'o']

-}
insertCharactersDelayingAdjacentDuplicates :: Int -> Int -> String -> [CharacterInsertion]
insertCharactersDelayingAdjacentDuplicates defaultDelay duplicateDelay
  = group
  > fmap ( fmap InsertCharacter
         > intersperse (DelayMilliseconds duplicateDelay)
         )
  > intersperse [DelayMilliseconds defaultDelay]
  > concat
  > _optimize
  where
  _delayed :: Int -> [CharacterInsertion] -> [CharacterInsertion]
  _delayed j = intersperse (DelayMilliseconds j)
  _optimize :: [CharacterInsertion] -> [CharacterInsertion]
  _optimize = filter (/= DelayMilliseconds 0)

evaluateCharacterInsertions :: (MonadIO m) => [CharacterInsertion] -> m ()
evaluateCharacterInsertions = traverse_ evaluateCharacterInsertion
  where
  evaluateCharacterInsertion = \case
    InsertCharacter   c -> sendChar c
    -- InsertKey k -> pressKey k
    DelayMilliseconds i -> delayMilliseconds i

{-| for debugging.

>>> putStrLn $ displayCharacterInsertions (insertCharactersDelayingAdjacentDuplicates 1 30 "hello")
'h' 1 'e' 1 'l' 30 'l' 1 'o'

-}
displayCharacterInsertions :: [CharacterInsertion] -> String
displayCharacterInsertions
  = fmap go
  > intercalate " "
  where
  go = \case
    InsertCharacter c -> show c
--     InsertKey k -> show k
    DelayMilliseconds i -> show i

{-| send any character, including Unicode, to the active window.

TODO verify Unicode

-}
sendChar :: (MonadIO m) => Char -> m ()
sendChar c = liftIO $ do
 _ <- c_SendUnicodeChar (char2cwchar c)
 return ()

-- | cast doesn't overflow, there are ~1,000,000 chars.
char2cwchar :: Char -> CWchar
char2cwchar = CWchar . fromIntegral . ord

--------------------------------------------------------------------------------

-- sendString :: (MonadIO m) => String -> m ()
-- sendString s = liftIO $ withArrayLen s $ \size characters -> do
--   inputs <- todo
--   _ <- c_SendUnicodeString size characters inputs
--   return ()

--------------------------------------------------------------------------------

{-| inserts some text into the current Application.

@
@

TODO might work for the window of the virtual machine
-}
sendText_byKey  :: (MonadIO m) => String -> m ()
sendText_byKey = error"sendText_byKey"

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
currentApplication = Application <$> error "currentApplication" -- TODO

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

-- TODO
setCursorPosition' :: (MonadIO m) => POINT -> m ()
setCursorPosition' (POINT x y) = liftIO $ do
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
