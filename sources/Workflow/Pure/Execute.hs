{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}
module Workflow.Pure.Execute where
import Workflow.Types

import Control.Monad.Free
-- import Control.Monad.Trans.Free hiding (Pure, Free, iterM) -- TODO

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Maybe
import Data.Foldable                  (traverse_)
import Data.List                      (intercalate)
import Data.Monoid                    ((<>))
import Data.Function                    ((&))


{-| downcasts a monad to a list of functors.

e.g.

>>> isSimpleWorkflow $ getClipboard >>= sendText
Nothing

>>> isSimpleWorkflow $ setClipboard "copying..." >> sendKeyChord [HyperModifier] CKey
Just [SetClipboard "copying..." (),SendKeyChord [HyperModifier] CKey ()]

TODO When 'Just':

@
'isSimpleWorkflow' >>> fromJust >>> 'fromWorkflow_' === 'id'
@

-}
isSimpleWorkflow :: Workflow x -> Maybe [Workflow_]
isSimpleWorkflow m = (runIdentity . runMaybeT . execWriterT) (goM m)

 where
 log = tell . (:[])

 goM :: Workflow x -> SimpleWorkflowM ()
 goM = \case
  Pure x -> return ()
  Free a -> goF a

 goF :: WorkflowF (Workflow x) -> SimpleWorkflowM ()
 goF = \case

  -- simple
  SendKeyChord    flags key k -> log (SendKeyChord_    flags key) >> goM k
  SendText        s         k -> log (SendText_        s        ) >> goM k
  SendMouseClick    flags n button k -> log (SendMouseClick_    flags n button) >> goM k
  SendMouseScroll   flags scroll n k -> log (SendMouseScroll_   flags scroll n) >> goM k
  SetClipboard    s k         -> log (SetClipboard_    s  ) >> goM k
  OpenApplication app k       -> log (OpenApplication_ app) >> goM k
  OpenURL         url k       -> log (OpenURL_         url) >> goM k
  Delay           t k         -> log (Delay_           t  ) >> goM k

  -- complex
  GetClipboard _ -> mzero
  CurrentApplication _ -> mzero

type SimpleWorkflowM = WriterT [Workflow_] (MaybeT Identity)

{- | shows (an inaccurate approximation of) the
"static" data flow of some 'Workflow',
by showing its primitive operations (in @do-notation@).

e.g.

>>> :{
putStrLn . showWorkflow $ do
 sendKeyChord [Command, Shift] BKey
 delay 1000
 sendKeyChord [Command] DownArrowKey
 x1 <- currentApplication
 x2 <- getClipboard
 openURL $ "https://www.google.com/search?q=" <> x2
 setClipboard x1
 getClipboard
:}
do
 sendKeyChord ([Command,Shift]) (BKey)
 delay (1000)
 sendKeyChord ([Command]) (DownArrowKey)
 x1 <- currentApplication
 x2 <- getClipboard
 openURL ("https://www.google.com/search?q={x2}")
 setClipboard ("{x1}")
 x3 <- getClipboard
 return "{x3}"

(note: doesn't print variables as raw strings (cf. 'print' versus 'putStrLn'), as it doesn't "crystallize" all operations into "symbols", but gives you an idea of the data flow. however, it does correctly track the control flow, even when the variables are used non-sequentially.)

(note: the variables in the code were named to be consistent with
'gensym', for readability. but of course the bindings aren't reified,
and they could have been named anything)

basically, the monadically-bound variable @x1@ is shown as if it were literally @"{x1}"@ (rather than, the current clipboard contents). a more complicated alternative could be to purely model the state: e.g. a clipboard, with 'SetClipboard' and 'GetClipboard' working together, etc.).

TODO would be complicated by SendTextTo; unless unit consuctors are
distinguished (as sendTextTo =<< currentApplication is kinda Applicative).

-}
showWorkflow :: (Show x) => Workflow x -> String
showWorkflow m = "do\n" <> (evalState&flip) 1 (showWorkflow_ m)

 where
 showWorkflow_ :: (Show x) => Workflow x -> GensymM String
 showWorkflow_ = \case
  Pure x -> return $ " return " <> show x <> "\n"
  Free a -> showWorkflowF a

 showWorkflowF :: (Show x) => WorkflowF (Workflow x) -> GensymM String
 showWorkflowF = \case
  SendKeyChord    flags key k -> ((" sendKeyChord "    <> showArgs [show flags, show key]) <>)       <$> showWorkflow_ k
  -- TODO SendMouseClick  flags n b k -> ((" sendMouseClick "  <> showArgs [show flags, show n, show b]) <>) <$> showWorkflow_ k
  SendText        s k         -> ((" sendText "        <> showArgs [show s]) <>)                     <$> showWorkflow_ k

  SendMouseClick    flags n button k -> ((" sendMouseClick "     <> showArgs [show flags, show n, show button]) <>)       <$> showWorkflow_ k
  SendMouseScroll   flags scroll n k -> ((" sendMouseScroll "    <> showArgs [show flags, show scroll, show n]) <>)       <$> showWorkflow_ k

  SetClipboard    s k         -> ((" setClipboard "    <> showArgs [show s]) <>)                     <$> showWorkflow_ k
  OpenApplication app k       -> ((" openApplication " <> showArgs [show app]) <>)                   <$> showWorkflow_ k
  OpenURL         url k       -> ((" openURL "         <> showArgs [show url]) <>)                   <$> showWorkflow_ k
  Delay           t k         -> ((" delay "           <> showArgs [show t]) <>)                     <$> showWorkflow_ k

 -- TODO distinguish between strings and variables to avoid:
 -- x2 <- getClipboard
 -- sendText ("x2")

  GetClipboard f -> do
   x <- gensym
   rest <- showWorkflow_ (f ("{"<>x<>"}"))
   return $ " " <> x <> " <- getClipboard" <> showArgs [] <> rest

  CurrentApplication f -> do
   x <- gensym
   rest <- showWorkflow_ (f ("{"<>x<>"}"))
   return $ " " <> x <> " <- currentApplication" <> showArgs [] <> rest

 showArgs :: [String] -> String
 showArgs xs = intercalate " " (fmap (("(" <>) . (<> ")")) xs) <> "\n"

 gensym :: State Int String
 gensym = do
  i <- get
  put $ i + 1
  return $ "x" <> show i

type GensymM = State Gensym

type Gensym = Int

{-

 \case

 SendKeyChord    flags key k  ->  k
 SendText        s k          ->  k
 SendMouseClick    flags n button k ->  k
 SendMouseScroll   flags scroll n k ->  k
 SetClipboard    s k         ->  k
 OpenApplication app k       ->  k
 OpenURL         url k       ->  k
 Delay           t k         ->  k

 GetClipboard f ->  f
 CurrentApplication f ->  f

-}
