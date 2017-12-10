{-# OPTIONS_GHC -fno-warn-missing-signatures #-} -- to test inference
module Workflow.Backends.X11.Example where
import Workflow.Backends.X11

import System.Exit (exitWith, ExitCode(..))
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
 threadDelay (1 * 1000000)

 getClipboard' >>= print


 
