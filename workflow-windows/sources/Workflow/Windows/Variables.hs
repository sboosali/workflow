{-|

integration with the `StateVar` package.

e.g.

@
import Foreign.Var

reverseClipboard = do
 'clipboard' '$~' reverse

moveCursorToTopLeft = do
 'cursor' '$=' POINT 0 0
@

-}
module Workflow.Windows.Variables where
import Workflow.Windows.Types
import Workflow.Windows.Bindings

import Data.StateVar

clipboard :: StateVar String
clipboard = StateVar getClipboard setClipboard

cursor :: StateVar POINT
cursor = StateVar getCursorPosition setCursorPosition -- moveMouse

-- application :: StateVar String
-- application = currentApplication launchApplication
--
-- window :: StateVar Window
-- window = currentWindow activateWindow
