{-|

== Partiality

The low-level functions in  'Workflow.Backends.X11.Foreign' don't throw exceptions whenever possible, instead returning any standard error message and the exit code, and thus can be easily\/explicitly handled. For example, if a command (e.g. @xdotool@) exits with @127@ (i.e. "command not found"), one could detect the system package manager and attempt to install the appropriate package (e.g. @nix-env -i xdotool@).

The higher-level functions in 'Workflow.Backends.X11.Bindings' and 'Workflow.Backends.X11.Execute' throw exceptions for anything unexpected, and thus are bugs that should be reported ( <https://github.com/sboosali/workflow-x11/issues> ). This potentially includes but isn't limited to: ill-typed output, a shadowed executable, a version mismatch, or race conditions. (Currently, there are no bugs that affect my daily usage of this package).

== Examples

@
@

== Dependencies

Shells out to the following executables:

* @xdotool@
* @xclip@
* @xdg-open@

You can statically ensure that these dependencies are installed by building the package with `stack --nix`. You can also dynamically double-check with 'checkExternalDependencies', which simply check that these executable names are on the path.

-}
module Workflow.Backends.X11
 ( module Workflow.Backends.X11.Types
 , module Workflow.Backends.X11.Bindings
 ) where
import Workflow.Backends.X11.Types
import Workflow.Backends.X11.Bindings
