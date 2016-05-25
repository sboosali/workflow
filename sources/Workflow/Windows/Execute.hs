{-# LANGUAGE LambdaCase, ScopedTypeVariables, FlexibleContexts #-}
module Workflow.Windows.Execute where
import Workflow.Windows.Constants
import Workflow.Windows.Bindings as Win32
import Workflow.Windows.Types
import Workflow.Windows.Extra
import Workflow.Types hiding (Application,URL)

import Control.Monad.Free
import Control.Monad.Trans.Free hiding (Pure, Free, iterM) -- TODO

import Control.Monad.IO.Class


runWorkflow :: Workflow a -> IO a
runWorkflow = runWorkflowT . toFreeT

{-|

you can eliminate a custom monad:

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

with:

@
runW :: W a -> IO a
runW = 'runMonadWorkflow' . getW
@

-}
runWorkflowT :: forall m a. (MonadIO m) => WorkflowT m a -> m a
runWorkflowT = iterT go
 where
 go :: WorkflowF (m a) -> m a
 go = \case

  SendKeyChord    modifiers key k  -> win32SendKeyChord modifiers key >> k
  SendText        s k              -> Win32.sendText s >> k
  -- TODO support Unicode by inserting "directly"
  -- terminates because sendTextAsKeypresses is exclusively a sequence of SendKeyChord'es

  -- TODO SendMouseClick  flags n button k -> Win32.clickMouse flags n button >> k

  GetClipboard    f                -> Win32.getClipboard >>= f
  SetClipboard    s k              -> Win32.setClipboard s >> k

  CurrentApplication f             -> Win32.currentApplication >>= (getApplication >>> f)
  OpenApplication app k            -> Win32.openApplication (Application app) >> k
  OpenURL         url k            -> Win32.openUrl (URL url) >> k

  Delay           t k              -> delayMilliseconds t >> k
 -- 1,000 µs is 1ms

--------------------------------------------------------------------------------

win32SendKeyChord :: (MonadIO m) => [Modifier] -> Key -> m ()
win32SendKeyChord modifiers key
 = liftIO $ Win32.pressKeychord (fromModifier <$> modifiers) (fromKey key)

{-|

-}
fromModifier :: Modifier -> VK
fromModifier = \case
 MetaModifier     -> VK_MENU
 HyperModifier    -> VK_CONTROL
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
