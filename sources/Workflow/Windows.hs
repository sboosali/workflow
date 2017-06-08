{-| Automate keyboard\/mouse\/clipboard\/application interaction.

-}
module Workflow.Windows
 (
   -- |  WinAPI types
   module Workflow.Windows.Types

   -- | Low-level, C Foreign functions
 , module Workflow.Windows.Foreign

   -- | Medium-level, direct Haskell bindigs
 , module Workflow.Windows.Bindings

   -- | Higher-level bindings, for the platform-agnostic @workflow@ package.
 , module Workflow.Windows.Execute

  -- | Pattern synonyms
 , module Workflow.Windows.Constants

 -- | @StateVar@s
 , module Workflow.Windows.Variables

 -- | a long list of constants for Windows system error codes
 , module Workflow.Windows.Error

 ) where
import Workflow.Windows.Types
import Workflow.Windows.Constants
import Workflow.Windows.Foreign
import Workflow.Windows.Bindings
import Workflow.Windows.Execute
import Workflow.Windows.Variables
import Workflow.Windows.Error
