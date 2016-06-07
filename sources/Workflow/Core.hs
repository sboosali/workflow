{-|

-}
module Workflow.Core
 ( module Workflow.Types
 , module Workflow.Execute
 , module Workflow.Derived
 , module Workflow.Keys
 )
 where
import Workflow.Types
import Workflow.Execute
import Workflow.Derived
import Workflow.Keys

{-TODO (Doesn't work)

Re-exports "Control.Monad.Catch", instances only.
By re-exporting these instances ourselves (e.g. @instance MonadThrow IO@),
we save clients from explicitly depending on the @exceptions@ package
in their @build-depends@.

 , module Control.Monad.Catch
import qualified Control.Monad.Catch ()

-}
