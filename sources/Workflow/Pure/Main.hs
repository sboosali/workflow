module Workflow.Pure.Main where
import Workflow.Pure
import Workflow.Types

{-
stack build && stack exec -- workflow-pure-example
-}
main :: IO ()
main = do
 putStrLn "\nWorkflow.Pure...\n"

 let as = isSimpleWorkflow $ getClipboard >>= sendText
 case as of
   Nothing -> do
     putStrLn "isSimpleWorkflow succeeded"
   _ -> do
     putStrLn "isSimpleWorkflow failed"

 -- No instance for (Show (WorkflowF ()))
 -- Just [SetClipboard "copying..." (),SendKeyChord [HyperModifier] CKey ()]

 let as = isSimpleWorkflow $ setClipboard "copying..." >> sendKeyChord [HyperModifier] CKey
 print $ length as
 case as of
   Just [SetClipboard "copying..." (),SendKeyChord [HyperModifier] CKey ()] -> do
     putStrLn "isSimpleWorkflow succeeded"
   _ -> do
     putStrLn "isSimpleWorkflow failed"
