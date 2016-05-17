{-# LANGUAGE OverloadedStrings #-}
module Workflow.Windows.Main where
import Workflow.Windows
import Workflow.Windows.Extra

testWorkflow :: IO ()
testWorkflow = do
 contents <- getClipboard
 setClipboard (reverse contents)
 print contents
 sendChar 'c'
 sendText "sendText"
 pressKeychord windowsKeyboard [VK_CONTROL] VK_A -- press "C-a"
 openApplication "cmd.exe"
 openUrl "http://google.com"
 --clickMouseAt windowsMouse (POINT (maxBound `div` 2) minBound) 2 MOUSEEVENTF_LEFTDOWN MOUSEEVENTF_LEFTUP
 clickMouseAt windowsMouse (POINT 800 10) 2 MOUSEEVENTF_LEFTDOWN MOUSEEVENTF_LEFTUP
 hs_ScrollMouseWheel VerticalWheel Backwards 120

main :: IO ()
main = do
 nothing
 testWorkflow
