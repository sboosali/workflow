{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Workflow.Example where
--import Workflow.Types
import Workflow.Keys

{-
stack build && stack exec -- workflow-types-example
-}

main = do
  print $ readEmacsKeySequence "H-S-t H-l"  -- "re-open tab", then "jump to url bar"

