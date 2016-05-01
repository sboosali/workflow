{-# LANGUAGE OverloadedStrings #-}
module Workflow.Windows.Main where
import Workflow.Windows

testWorkflow :: IO ()
testWorkflow = do
 print =<< getClipboard
 sendChar 'c'
 sendText "insertText"
 pressKeychord windowsKeyboard [VK_CONTROL] VK_A
 openUrl "http://google.com"
 openApplication "cmd.exe"
 --clickMouseAt windowsMouse (Point (maxBound `div` 2) minBound) 2 MOUSEEVENTF_LEFTDOWN MOUSEEVENTF_LEFTUP
 clickMouseAt windowsMouse (Point 800 10) 2 MOUSEEVENTF_LEFTDOWN MOUSEEVENTF_LEFTUP

nothing :: IO ()
nothing = return ()

main :: IO ()
main = testWorkflow
