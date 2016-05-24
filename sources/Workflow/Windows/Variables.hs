{-|

integration with the `foreign-var` package.

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

import Foreign.Var

clipboard :: Var String
clipboard = Var getClipboard setClipboard

cursor :: Var POINT
cursor = Var getCursorPosition setCursorPosition -- moveMouse

-- application :: Var String
-- application = currentApplication launchApplication
--
-- window :: Var Window
-- window = currentWindow activateWindow
