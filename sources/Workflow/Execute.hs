{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, RecordWildCards #-}
module Workflow.Execute where
import Workflow.Types

import Control.Monad.Trans.Free (iterT)

import Control.Monad.IO.Class
import Control.Concurrent (threadDelay)
import Numeric.Natural


{-| an explicit type-class "dictionary" for interpreting a 'MonadWorkflow'.

e.g.

@
WorkflowD IO
@

-}
data WorkflowD m = WorkflowD
 { _SendKeyChord       :: [Modifier] -> Key -> m ()
 , _SendText           :: String            -> m ()

 , _SendMouseClick     :: [Modifier] -> Natural     -> MouseButton -> m ()
 , _SendMouseScroll    :: [Modifier] -> MouseScroll -> Natural     -> m ()

 , _GetClipboard       :: m Clipboard
 , _SetClipboard       :: (Clipboard -> m ())

 , _CurrentApplication :: m Application
 , _OpenApplication    :: Application -> m ()

 , _OpenURL            :: URL -> m ()

 , _Delay              :: (MilliSeconds -> m ())

 } -- deriving (Functor)

--------------------------------------------------------------------------------

{-|

e.g.

@
shellDictionary :: WorkflowD IO
shellDictionary = WorkflowD{..}
 where
 _GetClipboard = shell $ "pbpaste"
 _SetClipboard s = shell $ "echo "++(shellEscape s)++"| pbcopy" >> return ()
 ...

runWorkflowByShell :: (MonadIO m) => 'WorkflowT' m a -> m a
runWorkflowByShell = runWorkflowWithT shellDictionary

-- specializeable:
-- runWorkflowByShell :: 'Workflow' a -> IO a
@

-}
runWorkflowWithT
  :: forall m a. (MonadIO m)
  -- => CoWorkflowT (m a)
  => WorkflowD m
  -> WorkflowT m a
  -> m a
-- runWorkflowWithT CoWorkflowF{..} = iterT go
runWorkflowWithT WorkflowD{..} = iterT go
 where

 go :: WorkflowF (m a) -> m a
 go = \case

  SendKeyChord    flags key k      -> _SendKeyChord flags key >> k
  SendText        s k              -> _SendText s             >> k

  SendMouseClick  flags n button k    -> _SendMouseClick flags n button     >> k
  SendMouseScroll flags scrolling n k -> _SendMouseScroll flags scrolling n >> k

  GetClipboard    f                -> _GetClipboard   >>= f
  SetClipboard    s k              -> _SetClipboard s >>  k

  CurrentApplication f             -> _CurrentApplication  >>= f
  OpenApplication app k            -> _OpenApplication app >>  k
  OpenURL         url k            -> _OpenURL url         >>  k

  Delay           t k              -> delayMilliseconds t >> k
 -- 1,000 µs is 1ms

delayMilliseconds :: (MonadIO m) => Int -> m ()
delayMilliseconds = liftIO . threadDelay . (*1000)
