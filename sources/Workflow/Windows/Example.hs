{-# LANGUAGE OverloadedStrings, NegativeLiterals, ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{- | (Read the source)

run with:

@
stack build && stack exec -- example-workflow-windows
@

TODO is has segfaulted once, maybe while quickly switching applications?

-}
module Workflow.Windows.Example where
import Workflow.Windows
import Workflow.Windows.Extra

import Data.StateVar

import System.Environment
import Prelude.Spiros (delayMilliseconds)


testWhitespace i j = do -- originally didn't work
  let is = insertCharactersDelayingAdjacentDuplicates i j "x\n\tx"
  print $ is
  putStrLn $ displayCharacterInsertions is
  delayMilliseconds 1000
  sendTextByCharacterDelayingAdjacentDuplicates i j "x\n\tx"

testText i j = do
  putStrLn "\n- inserting text dupliecate characters, with and without a delay betwen each character ...\n"

  putStrLn ""
  let is = insertCharactersDelayingAdjacentDuplicates i j "hello"
  print $ is
  putStrLn $ displayCharacterInsertions is

  delayMilliseconds 1000
  sendTextByCharacterDelayingAdjacentDuplicates i j "abcdefghijklmnopqrtuvwxyzzzzzzzzzzzzzzzzzzzzzzzzzzz "

  -- putStrLn "0ms"
  --sendTextDelaying_byChar 0    "llama, hello" -- (0ms delay)"
  -- putStrLn ">0ms"
  -- delayMilliseconds 1000
  -- sendTextDelaying_byChar i "abcdefghijklmnopqrtuvwxyzzzzzzzzzzzzzzzzzzzzzzzzzzz " -- (>0ms delay)"


testVariables = do
  putStrLn "\n- vars...\n"

  reverseClipboard
  print =<< get clipboard

  moveCursorToTopLeft
  print =<< get cursor

reverseClipboard = do
 clipboard $~ reverse

moveCursorToTopLeft = do
 cursor $= POINT 0 0

{-
stack build && stack exec workflow-windows-example
-}
testWorkflow = do
 contents <- getClipboard
 setClipboard (reverse contents)
 print contents
 sendChar 'c'
 sendText "sendText"
 pressKeyChord [VK_CONTROL] VK_A -- press "C-a"
 openApplication "cmd.exe"
 openUrl "http://google.com"
 --clickMouseAt windowsMouse (POINT (maxBound `div` 2) minBound) 2 MOUSEEVENTF_LEFTDOWN MOUSEEVENTF_LEFTUP
 clickMouseAt (POINT 800 10) 2 MOUSEEVENTF_LEFTDOWN MOUSEEVENTF_LEFTUP
 scrollMouse MOUSEEVENTF_WHEEL 1 120 -- up (with my trackpad, "natural" scrolling disabled)
 delayMilliseconds 1000
 scrollMouse MOUSEEVENTF_WHEEL -1 60 -- down (with my trackpad, "natural" scrolling disabled)

testWindow s = do
 putStrLn $ "\nwindowClass: " ++ s
 w <- findWindow (aWindowClass s)
 e <- c_GetLastError
 putStr "GetLastError: "
 print e
 putStr "non-null: "
 print $ not (isNull (getHWND w))
 getWindowRectangle w >>= print

main = do
 putStrLn "\nworkflow-windows-example...\n"

 -- putStr "\ndebug Privileges:"
 -- print =<< c_EnableDebugPriv -- doesnt seem to be necessary

 -- testVariables

 testInlineC 

 --delayMilliseconds 1000
 -- getArgs >>= go

 where
 go = \case
   [read -> i, read -> j] -> do
     -- testText i j
     testWhitespace i j

 -- delayMilliseconds 1000
 -- scrollMouse MOUSEEVENTF_WHEEL 1 120 -- up (with my trackpad, "natural" scrolling disabled)
 -- delayMilliseconds 1000
 -- scrollMouse MOUSEEVENTF_WHEEL -1 60 -- down (with my trackpad, "natural" scrolling disabled)
 -- NOTE `-1::DWORD` triggers a warning, but the overflow is expected by windows, and works.

 -- delayMilliseconds 4000
 --testWorkflow
 --pressKeyChord [] VK_VOLUME_MUTE
 -- pressKeyChord [] VK_MEDIA_PLAY_PAUSE
 -- pressKeyChord [] VK_MEDIA_PLAY_PAUSE
 -- replicateM_ 2 $ pressKeyChord [VK_MENU] VK_F -- press "A-f"

 -- traverse_ testWindow ["OpusApp", "Emacs", "ConsoleWindowClass", "Chrome_WidgetWin_1"]
  -- "OpusApp" no, "Emacs" no, "ConsoleWindowClass" no, "Chrome_WidgetWin_1"  yes -- (Window "" "Chrome_WidgetWin_1" "")

 -- pressKeyChord [] VK_OEM_PLUS  -- "="
 -- pressKeyChord [VK_SHIFT] VK_OEM_PLUS -- "+"

{- when minimized, negs:

windowClass: Chrome_WidgetWin_1
non-null: True
RECT {leftRECT = -31992, topRECT = -31941, rightRECT = -31928, bottomRECT = -31877}

-}
