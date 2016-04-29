{-# LANGUAGE OverloadedStrings #-}
module Workflow.Windows.Main where
import Workflow.Windows

main :: IO ()
main = do
 print =<< getClipboard
 sendChar 'c'
 sendText "insertText"
 pressKeychord vkKeyboard [VK_CONTROL] VK_A
 openUrl "http://google.com"
 openApplication "cmd.exe"
