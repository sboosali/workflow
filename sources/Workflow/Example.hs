{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Workflow.Example where
--import Workflow.Types
import Workflow.Keys

{-
stack build && stack exec -- workflow-types-example
-}

main = do
  print $ readKeyBinding "H-S-t"  -- "re-open tab"
  -- ([HyperModifier, ShiftModifier], TKey)
