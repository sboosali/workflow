module Workflow.Pure.Main where
import Workflow.Pure
import Workflow.Types

{-
stack build && stack exec -- workflow-pure-example
-}
main :: IO ()
main = do
 putStrLn "\nWorkflow.Pure...\n"

 -- Workflow_ fixes {No instance for (Show (WorkflowF ()))}
 print $ isSimpleWorkflow $ getClipboard >>= sendText
 -- Nothing
 print $ isSimpleWorkflow $ setClipboard "copying..." >> sendKeyChord [HyperModifier] CKey
 -- Just [SetClipboard "copying..." (),SendKeyChord [HyperModifier] CKey ()]
