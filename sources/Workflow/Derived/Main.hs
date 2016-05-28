module Workflow.Derived.Main where
import Workflow.Derived

main :: IO ()
main = do
 putStrLn ""

 {-

 No instance for (Control.Monad.Free.Class.MonadFree
                    Workflow.Types.WorkflowF IO)
   arising from a use of ‘google’

 ;)

 -}
 -- google "haskell workflow"
