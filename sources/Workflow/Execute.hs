{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, RecordWildCards #-}
module Workflow.Execute where
import Workflow.Types

import Control.Monad.Trans.Free (iterT)

import Control.Monad.IO.Class
import Control.Concurrent (threadDelay)
import Numeric.Natural


{-| An explicit type-class "dictionary" for interpreting a 'MonadWorkflow'.

i.e. a generic handler/interpreter (product type)
for 'WorkflowF' effects (a sum type).

e.g.

@
WorkflowD IO
@

template:

@
myDictionary :: (MonadIO m) => WorkflowD m
myDictionary = WorkflowD{..}
 where
  _sendKeyChord =
  _sendText     =

  _sendMouseClick  =
  _sendMouseScroll =

  _getClipboard =
  _setClipboard =

  _currentApplication =
  _openApplication    =
  _openURL            =

runWorkflowByMy :: (MonadIO m) => WorkflowT m a -> m a
runWorkflowByMy = runWorkflowByT myDictionary
@

'Delay' is elided, as its implementation can use
cross-platform 'IO' ('threadDelay').

see 'runWorkflowByT'

-}
data WorkflowD m = WorkflowD
 { _sendKeyChord       :: [Modifier] -> Key -> m ()
 , _sendText           :: String            -> m ()

 , _sendMouseClick     :: [Modifier] -> Natural     -> MouseButton -> m ()
 , _sendMouseScroll    :: [Modifier] -> MouseScroll -> Natural     -> m ()

 , _getClipboard       :: m Clipboard
 , _setClipboard       :: (Clipboard -> m ())

 , _currentApplication :: m Application
 , _openApplication    :: Application -> m ()

 , _openURL            :: URL -> m ()

 -- , _delay :: MilliSeconds -> m ()
 } -- deriving (Functor)

--------------------------------------------------------------------------------

{-|

e.g.

@
shellDictionary :: WorkflowD IO
shellDictionary = WorkflowD{..}
 where
 '_getClipboard' = shell $ "pbpaste"
 '_setClipboard' s = shell $ "echo "++(shellEscape s)++"| pbcopy" >> return ()
 ...

runWorkflowByShell :: (MonadIO m) => 'WorkflowT' m a -> m a
runWorkflowByShell = runWorkflowByT shellDictionary

-- specializeable:
-- runWorkflowByShell :: 'Workflow' a -> IO a
@

-}
runWorkflowByT
  :: forall m a. (MonadIO m)
  -- => CoWorkflowT (m a)
  => WorkflowD m
  -> WorkflowT m a
  -> m a
-- runWorkflowByT CoWorkflowF{..} = iterT go
runWorkflowByT WorkflowD{..} = iterT go
 where

 go :: WorkflowF (m a) -> m a
 go = \case

  SendKeyChord    flags key k      -> _sendKeyChord flags key >> k
  SendText        s k              -> _sendText s             >> k

  SendMouseClick  flags n button k    -> _sendMouseClick flags n button     >> k
  SendMouseScroll flags scrolling n k -> _sendMouseScroll flags scrolling n >> k

  GetClipboard    f                -> _getClipboard   >>= f
  SetClipboard    s k              -> _setClipboard s >>  k

  CurrentApplication f             -> _currentApplication  >>= f
  OpenApplication app k            -> _openApplication app >>  k
  OpenURL         url k            -> _openURL url         >>  k

  Delay           t k              -> delayMilliseconds t >> k
 -- 1,000 µs is 1ms

delayMilliseconds :: (MonadIO m) => Int -> m ()
delayMilliseconds = liftIO . threadDelay . (*1000)

delaySeconds :: (MonadIO m) => Int -> m ()
delaySeconds =  delayMilliseconds . (*1000)
