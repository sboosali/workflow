{-# LANGUAGE LambdaCase, ScopedTypeVariables, FlexibleContexts, ViewPatterns, RecordWildCards #-}
{-|

high-level bindings.

Glue between "Workflow.Types" and "Workflow.Windows.Types".

-}
module Workflow.Windows.Execute where
import Workflow.Windows.Constants
import Workflow.Windows.Bindings as Win32
import Workflow.Windows.Types as Win32
import Workflow.Windows.Extra
import Workflow.Core hiding (Application,getApplication,URL,delayMilliseconds)

import Control.Monad.Free
import Control.Monad.Trans.Free hiding (Pure, Free, iterM) -- TODO
--import Data.Default.Class

import Prelude()

--------------------------------------------------------------------------------

defaultWindowsExecuteWorkflow :: ExecuteWorkflow
defaultWindowsExecuteWorkflow = ExecuteWorkflow (runWorkflowWithT defaultWindowsWorkflowConfig)

{-|

All delays are in milliseconds.

-}
data WindowsWorkflowConfig = WindowsWorkflowConfig
 { windowsHowToSendText  :: HowToSendText
 , windowsStepDelay      :: Natural
 , windowsCharacterDelay :: Natural
 , windowsDuplicateCharacterDelay :: Natural
 }
 deriving (Show,Read,Eq,Ord,Data,Generic)
instance NFData   WindowsWorkflowConfig
instance Hashable WindowsWorkflowConfig

-- | 'defaultWindowsWorkflowConfig'
instance Default WindowsWorkflowConfig where
   def = defaultWindowsWorkflowConfig

{- | How to execute 'sendText'.

Comparison:

* SendTextByChar:
* SendTextByClipboard:

-}
data HowToSendText = SendTextByChar -- SendTextByKey -- SendTextByClipboard
 deriving (Show,Read,Eq,Ord,Data,Generic) -- ,Enum,Bounded
instance NFData   HowToSendText
instance Hashable HowToSendText

-- | 'defaultHowToSendText'
instance Default HowToSendText where
   def = defaultHowToSendText

{-|@
'windowsHowToSendText' = 'defaultWindowsHowToSendText'
'windowsStepDelay'     = 'defaultWindowsStepDelay'
@-}
defaultWindowsWorkflowConfig :: WindowsWorkflowConfig
defaultWindowsWorkflowConfig = WindowsWorkflowConfig{..}
 where
 windowsHowToSendText  = defaultHowToSendText
 windowsStepDelay      = defaultWindowsStepDelay
 windowsCharacterDelay = defaultWindowsCharacterDelay
 windowsDuplicateCharacterDelay = defaultWindowsDuplicateCharacterDelay

{-|

@
= 'SendTextByChar'
@-}
defaultHowToSendText :: HowToSendText
defaultHowToSendText = SendTextByChar

{-| no delay.

@=0@
-}
defaultWindowsStepDelay :: Natural
defaultWindowsStepDelay = 0

{-| a brief delay.

@=1@ms

-}
defaultWindowsCharacterDelay :: Natural
defaultWindowsCharacterDelay = 1

{-| a longer delay.

@=35@ms

10ms seems to be the minimum delay before which all duplicate characters are dropped.
(all except the very first pair. e.g. "llama" works with no delay, while "hello" needs longer.)
but it's unreliable.

35ms seems to be a long enough delay.

-}
defaultWindowsDuplicateCharacterDelay :: Natural
defaultWindowsDuplicateCharacterDelay = 35

--------------------------------------------------------------------------------

runWorkflow :: Workflow a -> IO a
runWorkflow = runWorkflowT . toFreeT

{-| eliminate a 'WorkflowT' layer.

e.g. some custom monad:

@
newtype W a = W
 { getW :: WorkflowT IO a
 } deriving
 ( MonadWorkflow
 , MonadIO
 , Monad
 , Applicative
 , Functor
 )
@

specializing:

@
runW :: W a -> IO a
runW = 'runWorkflowT' . getW
@

-}
runWorkflowT :: forall m a. (MonadIO m) => WorkflowT m a -> m a
runWorkflowT = runWorkflowWithT def

runWorkflowWithT :: forall m a. (MonadIO m) => WindowsWorkflowConfig -> WorkflowT m a -> m a
runWorkflowWithT config@WindowsWorkflowConfig{..}
   = _delay
 >>> runWorkflowByT _dictionary

 where
 _dictionary = windowsWorkflowD config
 _delay = case windowsStepDelay of
   0 -> id -- optimization
   t -> intersperseT (Delay (toInt t) ())  -- (`delay` is too general, requiring unnecessary constraints)

{-|

-}
windowsWorkflowD :: (MonadIO m) => WindowsWorkflowConfig -> WorkflowD m
windowsWorkflowD WindowsWorkflowConfig{..} = WorkflowD{..} --TODO use delays
 where

 _sendText = case windowsHowToSendText of
   SendTextByChar      -> Win32.sendTextByCharacterDelayingAdjacentDuplicates (toInt windowsCharacterDelay) (toInt windowsDuplicateCharacterDelay)
  --  SendTextByChar      -> Win32.sendTextDelaying_byChar (toInt defaultWindowsCharacterDelay)
--   SendTextByKey       -> Win32.sendText_byKey
--   SendTextByClipboard -> Win32.sendText_byClipboard

 _sendKeyChord = sendKeyChord_Win32

 _sendMouseClick  = clickMouse_Win32
 _sendMouseScroll = scrollMouse_Win32

 _getClipboard = Win32.getClipboard
 _setClipboard = Win32.setClipboard

 _currentApplication = Win32.getApplication <$> Win32.currentApplication
 _openApplication    = Application >>> Win32.openApplication
 _openURL            = URL >>> Win32.openUrl

-----------------------------------------------------------------------------------------

clickMouse_Win32
 :: (MonadIO m) => [Modifier] -> Natural -> MouseButton -> m ()
clickMouse_Win32 modifiers n button = liftIO $ do
  POINT x y <- getCursorPosition --TODO Point has Cursor case
  holdingKeys (fromModifier <$> modifiers) $ do
      clickMouseAt_Win32 button n (x,y)

clickMouseAt_Win32
 :: (MonadIO m) => MouseButton -> Natural -> (LONG,LONG) -> m ()
clickMouseAt_Win32 (encodeMouseButton -> (down, up)) n (x,y)
 = Win32.clickMouseAt (POINT x y) n down up
--TODO type for screen coordinates. more than point. abs/rel. (bounded instance for rel).

scrollMouse_Win32
 :: (MonadIO m) => [Modifier] -> MouseScroll -> Natural -> m ()
scrollMouse_Win32 modifiers (encodeMouseScroll -> (wheel, direction)) n
 = liftIO $ holdingKeys (fromModifier <$> modifiers) $ do
     Win32.scrollMouse wheel direction n

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
 -- XButton      -> (MOUSEEVENTF_XDOWN      , MOUSEEVENTF_XUP)

-----------------------------------------------------------------------------------------

sendKeyChord_Win32 :: (MonadIO m) => [Modifier] -> Key -> m ()
sendKeyChord_Win32 modifiers key
 = Win32.pressKeyChord (fromModifier <$> modifiers) (fromKey key)

{-|

-}
fromModifier :: Modifier -> VK
fromModifier = \case
-- "virtual, virtual" modifiers
 MetaModifier     -> VK_MENU
 HyperModifier    -> VK_CONTROL
 -- "actual, virtual" modifiers
 ControlModifier  -> VK_CONTROL
 OptionModifier   -> VK_MENU
 ShiftModifier    -> VK_SHIFT
 FunctionModifier -> VK_FN

{-|

-}
fromKey :: Key -> VK
fromKey = \case

 -- "virtual, virtual" keys

 MetaKey -> VK_MENU
 HyperKey -> VK_CONTROL

 -- "actual, virtual" keys

 ControlKey -> VK_CONTROL
 CapsLockKey -> VK_CAPITAL
 ShiftKey -> VK_SHIFT
 OptionKey -> VK_MENU
 FunctionKey -> VK_FN

 GraveKey -> VK_OEM_3
 MinusKey -> VK_OEM_MINUS
 EqualKey -> VK_OEM_PLUS
 DeleteKey -> VK_BACK
 ForwardDeleteKey -> VK_DELETE
 LeftBracketKey -> VK_OEM_4
 RightBracketKey -> VK_OEM_6
 BackslashKey -> VK_OEM_5
 SemicolonKey -> VK_OEM_1
 QuoteKey -> VK_OEM_7
 CommaKey -> VK_OEM_COMMA
 PeriodKey -> VK_OEM_PERIOD
 SlashKey -> VK_OEM_2

 TabKey -> VK_TAB
 SpaceKey -> VK_SPACE
 ReturnKey -> VK_RETURN

 LeftArrowKey -> VK_LEFT
 RightArrowKey -> VK_RIGHT
 DownArrowKey -> VK_DOWN
 UpArrowKey -> VK_UP

 AKey -> VK_A
 BKey -> VK_B
 CKey -> VK_C
 DKey -> VK_D
 EKey -> VK_E
 FKey -> VK_F
 GKey -> VK_G
 HKey -> VK_H
 IKey -> VK_I
 JKey -> VK_J
 KKey -> VK_K
 LKey -> VK_L
 MKey -> VK_M
 NKey -> VK_N
 OKey -> VK_O
 PKey -> VK_P
 QKey -> VK_Q
 RKey -> VK_R
 SKey -> VK_S
 TKey -> VK_T
 UKey -> VK_U
 VKey -> VK_V
 WKey -> VK_W
 XKey -> VK_X
 YKey -> VK_Y
 ZKey -> VK_Z

 ZeroKey -> VK_0
 OneKey -> VK_1
 TwoKey -> VK_2
 ThreeKey -> VK_3
 FourKey -> VK_4
 FiveKey -> VK_5
 SixKey -> VK_6
 SevenKey -> VK_7
 EightKey -> VK_8
 NineKey -> VK_9

 EscapeKey -> VK_ESCAPE
 F1Key -> VK_F1
 F2Key -> VK_F2
 F3Key -> VK_F3
 F4Key -> VK_F4
 F5Key -> VK_F5
 F6Key -> VK_F6
 F7Key -> VK_F7
 F8Key -> VK_F8
 F9Key -> VK_F9
 F10Key -> VK_F10
 F11Key -> VK_F11
 F12Key -> VK_F12
 F13Key -> VK_F13
 F14Key -> VK_F14
 F15Key -> VK_F15
 F16Key -> VK_F16
 F17Key -> VK_F17
 F18Key -> VK_F18
 F19Key -> VK_F19
 F20Key -> VK_F20

--------------------------------------------------------------------------------
