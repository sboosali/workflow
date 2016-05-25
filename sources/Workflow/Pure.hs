{-| pure interpretations:

* display workflows by patching system variables

-}
--TODO * run workflows with a crude model of the os
module Workflow.Pure
 ( module Workflow.Pure.Types
 , module Workflow.Pure.Execute
 ) where
import Workflow.Pure.Types
import Workflow.Pure.Execute
