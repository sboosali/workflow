# workflow-windows

Automate keyboard/mouse/clipboard/application interaction.

## Examples

```

-- Reverse the current clipboard contents
contents <- getClipboard
setClipboard (reverse contents)

-- Insert a character into the current application
-- Unicode works
sendChar 'ξ'
sendText "sendText = traverse_ sendChar"

-- press "C-a"
pressKeyChord [VK_CONTROL] VK_A

-- Launch an application from the command line
-- (Opens a new window)
openApplication "cmd.exe"

-- In the default browser, open a new tab and visit the URL.
openUrl "http://google.com"

-- Double-click, 800 "pixels" to the right and 10 "pixels" down
-- (i.e. where the "close window" button might be)
clickMouseAt (POINT 800 10) 2 MOUSEEVENTF_LEFTDOWN MOUSEEVENTF_LEFTUP

-- Scroll the mouse-wheel by 120 "ticks"
-- e.g. with my trackpad, "natural" scrolling disabled, "scrolls up"
scrollMouse MOUSEEVENTF_WHEEL 1 120

```
