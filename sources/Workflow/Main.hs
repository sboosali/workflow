module Workflow.Main where
import Workflow.Bindings

main :: IO ()
main = do
 print =<< getClipboard
 insertChar 'c'
 insertText "insertText"
