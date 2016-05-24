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

-----------------------------------------------------------------------------------------

win32SendKeyChord :: (MonadIO m) => [Modifier] -> Key -> m ()
win32SendKeyChord modifiers key
 = liftIO $ Win32.pressKeychord (fromModifier <$> modifiers) (fromKey key)

fromModifier :: Modifier -> VK
fromModifier = \case
 MetaModifier     -> VK_MENU
 HyperModifier    -> VK_CONTROL
 ControlModifier  -> VK_CONTROL
 OptionModifier   -> VK_MENU
 ShiftModifier    -> VK_SHIFT
 FunctionModifier -> VK_FN

fromKey :: Key -> VK
fromKey = \case
 _ -> todo
