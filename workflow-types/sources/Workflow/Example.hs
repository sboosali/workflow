{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Workflow.Example where
import Workflow.Core

{-
stack build && stack exec -- example-workflow-types
-}

main = do
  print $ readEmacsKeySequence "H-S-t H-l" -- "re-open tab", then "jump to url bar"

