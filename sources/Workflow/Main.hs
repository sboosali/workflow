{-# LANGUAGE OverloadedStrings #-}
module Workflow.Main where
import Workflow.Bindings
import Workflow.Windows.Types

main :: IO ()
main = do
 print =<< getClipboard
 sendChar 'c'
 sendText "insertText"
 pressKeychord vkKeyboard [VK_CONTROL] VK_A
 openUrl "http://google.com"
 openApplication "cmd.exe"
