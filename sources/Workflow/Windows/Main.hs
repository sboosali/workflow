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

testWindow s = do
 putStrLn $ "\nwindowClass: " ++ s
 w <- findWindow (aWindowClass s)
 e <- c_GetLastError
 putStr "GetLastError: "
 print e
 putStr "non-null: "
 print $ not (isNull (getHWND w))
 getWindowRectangle w >>= print

main :: IO ()
main = do
 putStrLn "\nworkflow-windows-example...\n"

 putStr "\ndebug Privileges:"
 print =<< c_EnableDebugPriv

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
 setCursorPosition (POINT 0 0)

 traverse_ testWindow ["OpusApp", "Emacs", "ConsoleWindowClass", "Chrome_WidgetWin_1"]
  -- "OpusApp" no, "Emacs" no, "ConsoleWindowClass" no, "Chrome_WidgetWin_1"  yes -- (Window "" "Chrome_WidgetWin_1" "")

{- when minimized, negs:

windowClass: Chrome_WidgetWin_1
non-null: True
RECT {leftRECT = -31992, topRECT = -31941, rightRECT = -31928, bottomRECT = -31877}

-}
