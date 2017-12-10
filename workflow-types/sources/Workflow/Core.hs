{-| Automate keyboard\/mouse\/clipboard\/application interaction.

the core types that define platform-diagnostic workflows,
as well as helpers for parsing keychords and implementing backends.

(see 'WorkflowF', 'MonadWorkflow', 'Key', 'press', 'WorkflowD')

-}
module Workflow.Core
 ( module Workflow.Types
 , module Workflow.Lens
 , module Workflow.Execute
 , module Workflow.Keys
 , module Workflow.Reexports
 )
 where
import Workflow.Types
import Workflow.Lens
import Workflow.Execute
import Workflow.Keys

{-TODO (Doesn't work)

Re-exports "Control.Monad.Catch", instances only.
By re-exporting these instances ourselves (e.g. @instance MonadThrow IO@),
we save clients from explicitly depending on the @exceptions@ package
in their @build-depends@.

 , module Control.Monad.Catch
import qualified Control.Monad.Catch ()

-}
import Workflow.Reexports
