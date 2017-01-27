{-# LANGUAGE LambdaCase, ScopedTypeVariables, FlexibleContexts, RecordWildCards, GeneralizedNewtypeDeriving #-}
module Workflow.X11.Execute where
import Workflow.X11.Extra
import Workflow.X11.Bindings as X11
import Workflow.X11.Types

import Workflow.Core

import Data.Default.Class

import Control.Monad.Free
import Control.Monad.Trans.Free (intersperseT)

import Control.Monad.IO.Class

--------------------------------------------------------------------------------

{-

{-|

All delays are in milliseconds.

-}
data X11WorkflowConfig = X11WorkflowConfig
 { x11HowToSendText :: HowToSendText
 , x11StepDelay     :: Natural
 }
 deriving (Show,Read,Eq,Ord,Data,Generic)
instance NFData X11WorkflowConfig

-- | 'defaultX11WorkflowConfig'
instance Default X11WorkflowConfig where
   def = defaultX11WorkflowConfig

{- | How to execute 'sendText'.

Comparison:

* SendTextByChar:
* SendTextByKey:
* SendTextByClipboard:

-}
data HowToSendText = SendTextByChar | SendTextByKey | SendTextByClipboard
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Data,Generic)
instance NFData HowToSendText

-- | 'defaultHowToSendText'
instance Default HowToSendText where
   def = defaultHowToSendText

{-|@
'x11HowToSendText' = 'defaultX11HowToSendText'
'x11StepDelay'     = 'defaultX11StepDelay'
@-}
defaultX11WorkflowConfig :: X11WorkflowConfig
defaultX11WorkflowConfig = X11WorkflowConfig{..}
 where
 x11HowToSendText = defaultHowToSendText
 x11StepDelay     = defaultX11StepDelay

{-|

@
= 'SendTextByKey'
@-}
defaultHowToSendText :: HowToSendText
defaultHowToSendText = SendTextByKey

{-| no delay.

@=0@
-}
defaultX11StepDelay :: Natural
defaultX11StepDelay = 0

-}

--------------------------------------------------------------------------------

{- | A natural transformation from workflows to io.

Default settings and no transformers, for convenience.

@= 'runWorkflow' 'defaultX11WorkflowConfig'@

-}
runWorkflowDefault :: Workflow a -> IO a
runWorkflowDefault = runWorkflow -- defaultX11WorkflowConfig

-- | @runWorkflow config = 'runWorkflowT' config . 'toFreeT'@
runWorkflow :: X11WorkflowConfig -> Workflow a -> IO a
runWorkflow config = runWorkflowT config . toFreeT

{-|

you can eliminate a custom monad:

@
{# LANGUAGE GeneralizedNewtypeDeriving #}

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
runWorkflowT :: forall m a. (MonadIO m) => X11WorkflowConfig -> WorkflowT m a -> m a
runWorkflowT config@X11WorkflowConfig{..}
   = _delay
 >>> runWorkflowByT _dictionary

 where
 _dictionary = x11WorkflowD config
 _delay = case x11StepDelay of
   0 -> id -- optimization
   t -> intersperseT (Delay (nat2ms t) ())  -- (`delay` is too general, requiring unnecessary constraints)

{-|

-}
x11WorkflowD :: (MonadIO m) => X11WorkflowConfig -> WorkflowD m
x11WorkflowD X11WorkflowConfig{..} = WorkflowD{..} --TODO use delays
 where

 _sendText = X11.sendText_byKey
   
 -- _sendText = case x11HowToSendText of
 --   SendTextByChar      -> X11.sendText_byChar
 --   SendTextByKey       -> X11.sendText_byKey
 --   SendTextByClipboard -> X11.sendText_byClipboard

 _sendKeyChord = X11.sendKey

 _sendMouseClick  = X11.sendMouseClick'
 _sendMouseScroll = error "TODO X11.sendMouseScroll"

 _getClipboard = X11.getClipboard'
 _setClipboard = X11.setClipboard'

 _currentApplication = X11.currentApplication'
 _openApplication    = X11.openApplication'
 _openURL            = X11.openURL'

--------------------------------------------------------------------------------
