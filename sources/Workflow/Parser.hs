{-# LANGUAGE LambdaCase, ScopedTypeVariables, RankNTypes #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-| A command line interface for manual integration testing.


-}
module Workflow.Parser where
import Workflow.Core

import Text.Earley as E

import Control.Applicative
import Control.Arrow
import Data.Char
import Data.Function
import System.IO
import Control.Monad.IO.Class


  -- forall x. Workflow x -> IO x
  -- No instance for (MonadThrow (Control.Monad.Free.Free WorkflowF)) arising from a use of ‘press’

type Workflow' = WorkflowT IO ()

data Action
 = Quit
 | Help
 | Stay (Workflow')
 | Pause Int (Workflow')
 | Action (Workflow')

-- Maybe (IO ())

{- |

prefix with a number to pause (for that many seconds) before execution.

prefix with "stay" to disable "alt-tab"ing before execution.

e.g.

@
> help
...
> stay paste
> copy                   # (Having selected some text topmost (besides the current) window)
> 1000 paste             # Wait a second before pasting
> quit
@

-}
cmdln :: ExecuteWorkflow -> IO ()
cmdln runWorkflow = do
 hSetBuffering stdout NoBuffering
 go

 where

 go = do
   putStr prompt
   s <- getLine
   (evalAction runWorkflow) s & \case
     Nothing -> return ()
     Just io -> do
       io
       go

 prompt = "> "

evalAction :: ExecuteWorkflow -> String -> Maybe (IO ())
evalAction runWorkflow = parseAction >>> runAction runWorkflow

runAction :: ExecuteWorkflow -> Action -> Maybe (IO ())
runAction (ExecuteWorkflow runWorkflow) = \case
 Quit -> Nothing
 Help -> Just help

 Stay w -> Just $ runWorkflow $ do
   w

 Pause t w -> Just $ runWorkflow $ do
   delay (t*1000)
   w

 Action w -> Just $ runWorkflow $ do
   press "H-<tab>"
   delay 300
   w

help = do
  putStrLn "help" --TODO

parseAction :: String -> Action
parseAction s = case fst (p (words s)) of
  (a:_) -> a
  _ -> Help
 where
 p = E.fullParses (E.parser gAction)
--parseAction s = case fst (E.parser (E.fullParses gAction) s) of

--gAction :: E.Grammar r Action
gAction = do

 let pWord = E.satisfy (const True)
 pWords :: E.Prod r e String String <- E.rule $
  unwords <$> some pWord

 int <- E.rule $
  read <$> E.satisfy (all isDigit)

 pWorkflow :: E.Prod r e String Workflow' <- E.rule $ empty

  <|> (readEmacsKeySequence >>> maybe (return()) sendKeySequence) <$ E.token "press" <*> pWords
  <|> (sendText) <$ E.token "insert" <*> pWords

  <|> (getClipboard >>= (liftIO . putStrLn)) <$ E.token "getClipboard"
  <|> (setClipboard) <$ E.token "setClipboard" <*> pWords

  -- <|> () <$> E.token ""
  -- <|> () <$> E.token ""

  <|> (currentApplication >>= (liftIO . putStrLn)) <$ E.token "currentApplication"
  <|> (openApplication) <$ E.token "open" <*> pWords
  <|> (openURL) <$ E.token "url" <*> pWords

  <|> (getClipboard >>= sendText) <$ E.token "paste"
--  <|> () <$> E.token ""

  -- <|> pure (E.token "paste") *> do
  --     getClipboard >>= sendText

 -- pAction :: E.Prod r e String Action <- E.rule $ empty
 pAction <- E.rule $ empty
  <|> Quit <$ E.token "quit"
  <|> Help <$ E.token "help"
  <|> Stay   <$ E.token "stay" <*> pWorkflow
  <|> Pause  <$> int <*> pWorkflow
  <|> Action <$> pWorkflow

 return pAction
