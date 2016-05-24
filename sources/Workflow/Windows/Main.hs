{-# LANGUAGE OverloadedStrings #-}
module Workflow.Windows.Main where
import Workflow.Windows
import Workflow.Windows.Extra

{-
stack build && stack exec workflow-windows-example
-}
testWorkflow :: IO ()
testWorkflow = do
 contents <- getClipboard
 setClipboard (reverse contents)
 print contents
 sendChar 'c'
 sendText "sendText"
 pressKeychord [VK_CONTROL] VK_A -- press "C-a"
 openApplication "cmd.exe"
 openUrl "http://google.com"
 --clickMouseAt windowsMouse (POINT (maxBound `div` 2) minBound) 2 MOUSEEVENTF_LEFTDOWN MOUSEEVENTF_LEFTUP
 clickMouseAt (POINT 800 10) 2 MOUSEEVENTF_LEFTDOWN MOUSEEVENTF_LEFTUP
 scrollMouse ScrollTowards 120 -- up (with my trackpad, "natural" scrolling disabled)
 delayMilliseconds 1000
 scrollMouse ScrollAway 60 -- down (with my trackpad, "natural" scrolling disabled)

main :: IO ()
main = do
 print "workflow-windows-example..."
 -- delayMilliseconds 4000
 --testWorkflow
 --pressKeychord [] VK_VOLUME_MUTE
 -- pressKeychord [] VK_MEDIA_PLAY_PAUSE
 -- pressKeychord [] VK_MEDIA_PLAY_PAUSE
 -- replicateM_ 2 $ pressKeychord [VK_MENU] VK_F -- press "A-f"
 -- scrollMouse ScrollTowards 200
 -- delayMilliseconds 1000
 -- scrollMouse ScrollAway 1000
 getCursorPosition >>= print
