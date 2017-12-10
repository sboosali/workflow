{-# LANGUAGE ConstraintKinds, FlexibleContexts, PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# OPTIONS_GHC -ddump-splices #-} -- for `makeFree`

{-|

-}
module Workflow.Lens where
import Workflow.Extra
import Workflow.Types

import           Control.Monad.Free.TH       (makeFree)
import           Control.Monad.Free          (MonadFree, liftF)

--------------------------------------------------------------------------------
makeFree ''WorkflowF
-- th staging: the spilce can only access previous declarations

-- | @= 'traverse_' 'sendKeyChord''@
sendKeySequence :: (MonadWorkflow m) => KeySequence -> m ()
sendKeySequence = traverse_ sendKeyChord'

-- | uncurried 'sendKeyChord'
sendKeyChord' :: (MonadWorkflow m) => KeyChord -> m ()
sendKeyChord' (ms,k) = sendKeyChord ms k

--------------------------------------------------------------------------------

fromWorkflows_ :: (MonadWorkflow m) => [Workflow_] -> m ()
fromWorkflows_ = traverse_ (liftF . fromWorkflow_)

fromWorkflow_ :: Workflow_ -> WorkflowF ()
-- fromWorkflow_ :: (MonadWorkflow m) => Workflow_ -> m ()
fromWorkflow_ = \case
  SendKeyChord_    flags key      -> SendKeyChord     flags key      ()
  SendText_        s              -> SendText         s              ()
  SendMouseClick_  flags n button -> SendMouseClick   flags n button ()
  SendMouseScroll_ flags scroll n -> SendMouseScroll  flags scroll n ()
  SetClipboard_    s              -> SetClipboard     s              ()
  OpenApplication_ app            -> OpenApplication  app            ()
  OpenURL_         url            -> OpenURL          url            ()
  Delay_           t              -> Delay            t              ()

