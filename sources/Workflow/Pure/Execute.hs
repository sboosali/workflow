{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}
module Workflow.Pure.Execute where
import Workflow.Types

import Control.Monad.Free
-- import Control.Monad.Trans.Free hiding (Pure, Free, iterM) -- TODO

import Control.Monad.Trans.State
import Data.Foldable                  (traverse_)
import Data.List                      (intercalate)
import Data.Monoid                    ((<>))


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
showWorkflow as = "do\n" <> evalState (showWorkflow_ as) 1

 where
 showWorkflow_ :: (Show x) => Workflow x -> State Gensym String
 showWorkflow_ (Pure x) = return $ " return " <> show x <> "\n"
 showWorkflow_ (Free a) = showWorkflowF a

 showWorkflowF :: (Show x) => WorkflowF (Workflow x) -> State Gensym String
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

type Gensym = Int
