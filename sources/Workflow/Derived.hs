{-# LANGUAGE NoMonomorphismRestriction #-}
{-|

-}
module Workflow.Derived where
import Workflow.Types

import Control.Monad.Trans.Free (intersperseT)
-- import Control.Monad.Free
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class
import Numeric.Natural
import Data.Function ((&))

--------------------------------------------------------------------------------
chord = sendKeyChord

-- | access the currently selected region from Haskell,
--  by copying to the clipboard, via the default keyboard shortcut.
-- ('wait's 30ms)
copy :: (MonadWorkflow m) => m String
-- copy t :: (MonadWorkflow m) => Natural -> m String
copy = do
 chord [HyperModifier] CKey --TODO press "H-c"
 delay 30 -- one "frame". TODO how long does it need to wait?
 getClipboard
 -- ((->) Natural)? Reader? Cont?

-- | paste via the default keyboard shortcut.
paste :: (MonadWorkflow m) => m ()
paste = do
 chord [HyperModifier] VKey --TODO press "H-v"

-- |
zoomInByKeyboard = chord [HyperModifier] EqualKey

-- |
zoomOutByKeyboard = chord [HyperModifier] MinusKey

--------------------------------------------------------------------------------
insert = sendText

-- | \"paste\" by directly inserting the current clipboard contents.
paste' :: (MonadWorkflow m) => m ()
paste' = do
  getClipboard >>= insert

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
click :: (MonadWorkflow m) => [Modifier] -> Natural -> MouseButton -> m ()
click = sendMouseClick
--click = sendMouseClick [] 1 LeftButton

{-
Click = sendMouseClick []  Button
-}

leftClick   = sendMouseClick [] 1 LeftButton
middleClick = sendMouseClick [] 1 MiddleButton
rightClick  = sendMouseClick [] 1 RightButton

doubleClick = sendMouseClick [] 2 LeftButton
tripleClick = sendMouseClick [] 3 LeftButton

controlClick = sendMouseClick [ControlModifier] 1 LeftButton
shiftClick   = sendMouseClick [ShiftModifier]   1 LeftButton

--------------------------------------------------------------------------------
scroll = sendMouseScroll

ticks = (*120)

-- | scroll one "tick" of the wheel.
scrollOnce wheel = scroll [] wheel (1&ticks)  -- TODO windows only?

-- |
zoomInByMouse = scroll [ControlModifier] ScrollTowards (1&ticks)

-- |
zoomOutByMouse = scroll [ControlModifier] ScrollAway (1&ticks)

flingAway = scroll [] ScrollAway (10&ticks)

--------------------------------------------------------------------------------
wait :: (MonadWorkflow m, Integral a) => a -> m ()
wait = delay . fromIntegral

--------------------------------------------------------------------------------

delayMilliseconds :: (MonadIO m) => Int -> m ()
delayMilliseconds = liftIO . threadDelay . (*1000)
--TODO put in a .Execute as a utility

{-| intersperse a delay between each action.

@
delayWorkflowT 1 $ do
 sendKeyChord [CommandModifier] VKey
 s <- getClipboard
 sendText s
@

is equivalent to:

@
do
 sendKeyChord [CommandModifier] VKey
 delay 1
 s <- getClipboard
 delay 1
 sendText s
@

-}
delayWorkflowT :: (Monad m) => Int -> WorkflowT m a -> WorkflowT m a
delayWorkflowT t = intersperseT (Delay t ())
