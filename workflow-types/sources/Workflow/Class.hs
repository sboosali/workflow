{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|

-}
module Workflow.Class where

import Data.Word (Word)

import Prelude.Spiros
import Data.String
import Control.Monad.Catch

--------------------------------------------------------------------------------

{-| millisecond resolution

-}
class (MonadThrow m, MonadClipboard m, MonadApplication m, MonadKeyboard m, MonadMouse m) => MonadWorkflow m where 

  delayMilliseconds :: Natural -> m ()
 
  sendKeysIn :: Application -> [Modifier] -> Key -> m ()
  sendKeysTo :: Window      -> [Modifier] -> Key -> m ()

  sendTextIn :: Application -> String -> m () 
  sendTextTo :: Window      -> String -> m ()

  openURL :: URL -> m ()


{- MonadThrow or MonadFail or just Monad?

-}


--------------------------------------------------------------------------------

{-|

@setClipboard x >> getClipboard@ should return x (modulo race conditions with other external clipboard manipulation). 

@setClipboard@ should be idempotent.

-}
class (Monad m) => MonadClipboard m where 
 getClipboard :: m String
 setClipboard :: String -> m ()


--------------------------------------------------------------------------------

{-|

@openApplication@ should be idempotent. 
it either launches the application if it's not running,
or focus on the application if it already is.

an @Application@ has one-or-more @Window@s.


-}
class (Monad m) => MonadApplication m where 

 getCurrentApplication :: m Application 

 openApplication :: Application -> m ()

 getCurrentWindow :: m Window

 reachWindow :: Window -> m ()

 getRunningApplications :: m [Application]

 getWindowsOf :: Application -> m [Window]

 getWindows :: m [Window]
 getWindows = getCurrentApplication >>= getWindowsOf


--------------------------------------------------------------------------------

{-|

-}
class (Monad m) => MonadKeyboard m where 

 pressKey :: [Modifier] -> Key -> m ()

 sendText :: String -> m ()
 -- sendText = traverse_ char2key

 -- getCurrentlyHeldModifiers :: [Modifier]

--------------------------------------------------------------------------------

{-| 

-}
class (Monad m) => MonadMouse m where 

  clickMouse  :: [Modifier] -> Natural -> MouseButton -> m ()
  scrollMouse :: [Modifier] -> Natural -> MouseScroll -> m ()

--------------------------------------------------------------------------------

{-|

-}
newtype Key = Key Word
 deriving (Eq,Ord,Show,Num,NFData,Hashable)

{-|

-}
newtype Modifier = Modifier Word
 deriving (Eq,Ord,Show,Num,NFData,Hashable)

{-|

-}
newtype MouseButton = MouseButton Word
 deriving (Eq,Ord,Show,Num,NFData,Hashable)

{-|

-}
newtype MouseScroll = MouseScroll Word
 deriving (Eq,Ord,Show,Num,NFData,Hashable)

-- newtype Clipboard = Clipboard String

{-|

@
scheme:[//[user[:password]@]host[:port]][/path][?query][#fragment]
@

-}
newtype URL = URL String
 deriving (Eq,Ord,Show,IsString,Semigroup,Monoid,NFData,Hashable)

{-|

-}
newtype Application = Application String
 deriving (Eq,Ord,Show,IsString,NFData,Hashable)

{-|

-}
newtype Window = Window String
 deriving (Eq,Ord,Show,IsString,NFData,Hashable)

--------------------------------------------------------------------------------