{-# LANGUAGE DeriveAnyClass, PatternSynonyms, ConstraintKinds, FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- {-# OPTIONS_GHC -ddump-splices #-}
-- for `makeFree`

{-|

-}
module Workflow.Types where
import Workflow.Extra

import Control.Monad.Trans.Free (FreeT)
import Control.Comonad.Trans.Cofree (CofreeT)
-- import Control.Monad.Free.Church  (F)
import           Control.Monad.Free          (MonadFree, Free, liftF)
import           Control.Monad.Free.TH       (makeFree)
import           Control.Comonad.Cofree (Cofree)
-- import Control.Comonad

import Numeric.Natural
--import GHC.Exts

--------------------------------------------------------------------------------
{-$ WorkflowF
-}

{-|

-}

{- | platform-agnostic workflows,
which can be interpreted by platform-specific bindings.

Naming: "WorkflowF" for "Workflow Functor".

currently, no error codes are returned (only @()@)).
this (1) simplifies bindings and
(2) saves the user from explicitly ignoring action results (e.g. @_ <- getClipboard@).
later, they can be supported,
alongside wrappers that return @()@ and throw @SomeException@
and provide the same simple API.
since the intented usage of workflows are as user-facing
(often user-written) scripts, and
the monad that satisifes MonadWorkflow will often satisify MonadIO too,
convenient partial functions that throw a helpful error message to stdout
(the error codes should be converted to their error messages) should suffice.
and either way, is strictly better for the user than ignoring,
as the exceptions can always be caught, or not displayed.


-}
data WorkflowF k
 = SendKeyChord    [Modifier] Key                  k -- ^ press the 'Key' while the 'Modifier's are held down. sent to the current application.
 --TODO | SendKeyChordTo Application    [Modifier] Key                   k -- ^
 --TODO | SendKeyChordTo Window    [Modifier] Key                   k -- ^
 -- versus unary: ([Modifier], Key)
 -- rn SendChord

 | SendText        String                          k -- ^ a logical grouping for: (1) unicode support (2) efficiency and (3) debugging. sent to the current application.
 --TODO | SendTextTo Application        String                           k -- ^
 --TODO | SendTextTo Window        String                           k -- ^
 -- sendText = sendTextTo =<< currentApplication

 | SendMouseClick  [Modifier] Natural MouseButton  k  -- ^ click the button, some number of times, holding down the modifiers
  -- derived, make method, not constructor. sent to the current application.
 --TODO | SendMouseClickTo Application  [Modifier] Int MouseButton  k  ^ -- sent to the current application.
 -- versus unary: ([Modifier], Natural, MouseButton)
 | SendMouseScroll  [Modifier] MouseScroll Natural  k  -- ^ spin the wheel, some number of units*, holding down the modifiers

 | GetClipboard                                     (Clipboard -> k)
 | SetClipboard       Clipboard                     k

 | CurrentApplication                               (Application -> k) -- ^ like getter
 | OpenApplication    Application                   k                  -- ^ like setter
 --TODO | GetApplications ([Application] -> k)

 --TODO | CurrentWindow                               (Window -> k)
 --TODO | ReachWindow Window                      k
 --TODO | GetWindows        Application                       ([Window] -> k)   -- ^ an 'Application' has some 'Window's (zero or more on OSX, one or more on Windows/Linux, I think).

 | OpenURL         URL                              k

 | Delay           MilliSeconds                             k -- interpreted as 'threadDelay' on all platforms; included for convenience

 deriving (Functor)
 -- deriving (Functor,Data)

--------------------------------------------------------------------------------

{-| the non-monadic subset of 'WorkflowF'.
i.e. all cases that return @()@, preserving the previous continuation.

Naming: "unit workflow", like "traverse_".

-}
data Workflow_
 = SendKeyChord_    [Modifier] Key
 | SendText_        String
 | SendMouseClick_  [Modifier] Natural MouseButton
 | SendMouseScroll_ [Modifier] MouseScroll Natural
 | SetClipboard_    Clipboard
 | OpenApplication_ Application
 | OpenURL_         URL
 | Delay_           MilliSeconds
 deriving (Show,Read,Eq,Ord,Data,Generic,NFData)

--------------------------------------------------------------------------------

{- | abstract interface.

a monad constraint for "workflow effects"
(just like @MonadState@ is for "state effects").
Can be used in any monad transformer stack that handles them.

e.g.


-}
type MonadWorkflow = MonadFree WorkflowF

-- {- | for convenience.
-- without loss of generality (I don't think) when declaring simple monadic effects (like Kleisli arrows).

-- e.g.

-- @
-- getClipboard :: 'AMonadWorkflow'      String  -- generalized
-- getClipboard :: ('MonadWorkflow' m) => m String  -- generalized
-- getClipboard :: Free 'WorkflowF'         String  -- specialized
-- @

-- -}
-- type AMonadWorkflow a = forall m. (MonadWorkflow m) => m a

-- {-| Naming: "a unit monad workflow".
-- -}
-- type AMonadWorkflow_ = (forall m. (MonadWorkflow m) => m ())

-- | concrete transformer.
type WorkflowT = FreeT WorkflowF

-- | concrete monad.
type Workflow = Free WorkflowF

--------------------------------------------------------------------------------

type Clipboard = String

type Application = String

type URL = String

type MilliSeconds = Int
-- newtype Time = Time Int  deriving (Show,Eq,Ord,Num)
-- units package
-- milliseconds

-- class IsString TODO needs Free WorkflowF, which must be lifted,
-- which isn't better than an explicit insert


{-|

>>> :set -XOverloadedStrings
>>> "contents" :: Clipboard
"contents"

-}
--newtype Clipboard_ = Clipboard String
 --deriving (Show,Read,Eq,Ord,IsString,Data,Generic,Semigroup,NFData)
-- data Clipboard = Clipboard { cbContents :: String, cbFormat :: ClipboardFormat }
-- instance IsString UnicodeTextFormat where fromString s = Clipboard s UnicodeTextFormat
-- GetClipboardContents
-- getClipboard = getClipboardContents<&>cbContents
-- runWorkflow must take a mapping (ClipboardFormat -> something)
--  maybe: phantom data RawClipboard (format :: ClipboardFormat) = Bytestring
--  with reflection class KnownClipboardFormat

{-|

>>> :set -XOverloadedStrings
>>> "Emacs" :: Application
"Emacs"

-}
--newtype Application_ = Application String
 --deriving (Show,Read,Eq,Ord,IsString,Data,Generic,Semigroup,NFData)

{-|

>>> :set -XOverloadedStrings
>>> "https://google.com/" :: URL
"https://google.com/"

-}
--newtype URL_ = URL String
  --deriving (Show,Read,Eq,Ord,IsString,Data,Generic,Semigroup,NFData)
--TODO refined

--------------------------------------------------------------------------------

{-| Operating systems always (?) support at least these mouse events.

Most mice have these three buttons, trackpads have left/right.

-}
data MouseButton
 = LeftButton
 | MiddleButton
 | RightButton
 --TODO | XButton -- https://msdn.microsoft.com/en-us/library/windows/desktop/gg153549(v=vs.85).aspx
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Data,Generic,NFData)

{-| Mouse wheel scrolling, vertically and horizontally.

'ScrollTowards':

* scrolls up when "natural scrolling" is disabled
* scrolls down when "natural scrolling" is enabled TODO check

-}
data MouseScroll
  = ScrollTowards -- ScrollUp (from user)
  | ScrollAway -- ScrollDown (from user)
  | ScrollLeft
  | ScrollRight
  deriving (Show,Read,Eq,Ord,Enum,Bounded,Data,Generic,NFData)

--------------------------------------------------------------------------------

-- {-| Represents @a@ being bound to a keyboard shortcut.
--
-- Naming:
-- <https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Commands.html>
--
-- -}
-- data KeyBinding a = KeyBinding KeySequence a
--TODO mv to commands-core or something

{-| a sequence of key chords make up a keyboard shortcut

Naming:
<https://www.emacswiki.org/emacs/KeySequence>

TODO rename KeySequence

-}
type KeyBinding = [KeyChord]
-- newtype KeySequence = KeySequence (NonEmpty KeyChord)
--
--TODO newtype for non-overlapping IsString
-- instance IsString KCs where fromString =
-- --TODO refined
--
-- press :: KCs -> m()
--

-- an (unordered, no-duplicates) sequence of key
-- chords make up a keyboard shortcut
-- not a Set for simplicity (e.g. to avoid imports and Ord constraints).
--really?

{- | represents joitly holding down all the modifiers
while individually press each key down and back up.

Naming: https://www.emacswiki.org/emacs/Chord

-}
type KeyChord = ([Modifier], [Key]) --TODO ([Modifier], NonEmpty Key)
-- data KeyChord = KeyChord [Modifier] Key
{- data KeyChord = KeyChord
 { kcModifiers :: [Modifier]
 , kcKey       :: Key
 }
-}

-- | appends a modifier
addMod :: Modifier -> KeyChord -> KeyChord
addMod m (ms, k) = (m:ms, k)
-- false positive nonexhaustive warning with the KeyChord pattern. fixed in ghc8?
-- addMod m (KeyChord ms k) = KeyChord (m:ms) k

{- | modifier keys are keys that can be "held".

the escape key is "pressed", not "held", it seems.
(possibly explains its behavior in your terminal emulator?)

@alt@ is 'OptionModifier'.

-}
data Modifier
 = MetaModifier
 | HyperModifier
 | ControlModifier
 | OptionModifier --TODO rn Option Alt
 | ShiftModifier
 | FunctionModifier
 deriving (Show,Read,Eq,Ord,Bounded,Enum,Data,Generic,NFData)

{- | a "cross-platform" keyboard:

* keys that exist on standard keyboards.
* plus, 'MetaKey' and 'HyperKey': virtual modifiers to abstract over
common keyboard shortcuts.

(let me know if you want a type to support cross-platform international keyboards,
i haven't looked into it. you can still use the
paltform-specific virtual-key-codes in the dependent packages:
@workflow-linux@, @workflow-osx@, and @workflow-windows@)

-}
data Key

 = MetaKey -- ^ fake key: Alt on Linux/Windows, Command on OSX
 | HyperKey -- ^ fake key: Control on Linux/Windows, Command on OSX
-- Control/Command both have C/O/N

 | ControlKey
 | CapsLockKey
 | ShiftKey
 | OptionKey
 | FunctionKey

 | GraveKey
 | MinusKey
 | EqualKey
 | DeleteKey
 | ForwardDeleteKey
 | LeftBracketKey
 | RightBracketKey
 | BackslashKey
 | SemicolonKey
 | QuoteKey
 | CommaKey
 | PeriodKey
 | SlashKey

 | TabKey
 | SpaceKey
 | ReturnKey

 | LeftArrowKey
 | RightArrowKey
 | DownArrowKey
 | UpArrowKey

 | AKey
 | BKey
 | CKey
 | DKey
 | EKey
 | FKey
 | GKey
 | HKey
 | IKey
 | JKey
 | KKey
 | LKey
 | MKey
 | NKey
 | OKey
 | PKey
 | QKey
 | RKey
 | SKey
 | TKey
 | UKey
 | VKey
 | WKey
 | XKey
 | YKey
 | ZKey

 | ZeroKey
 | OneKey
 | TwoKey
 | ThreeKey
 | FourKey
 | FiveKey
 | SixKey
 | SevenKey
 | EightKey
 | NineKey

 | EscapeKey
 | F1Key
 | F2Key
 | F3Key
 | F4Key
 | F5Key
 | F6Key
 | F7Key
 | F8Key
 | F9Key
 | F10Key
 | F11Key
 | F12Key
 | F13Key
 | F14Key
 | F15Key
 | F16Key
 | F17Key
 | F18Key
 | F19Key
 | F20Key

 deriving (Show,Read,Eq,Ord,Bounded,Enum,Data,Generic,NFData)

--------------------------------------------------------------------------------
makeFree ''WorkflowF
-- th staging: the spilce can only access previous declarations

--------------------------------------------------------------------------------

fromWorkflows_ :: (MonadWorkflow m) => [Workflow_] -> m ()
fromWorkflows_ = traverse_ (liftF . fromWorkflow_)

fromWorkflow_ :: Workflow_ -> WorkflowF ()
-- fromWorkflow_ :: (MonadWorkflow m) => Workflow_ -> m ()
fromWorkflow_ = \case
  SendKeyChord_    flags key      -> SendKeyChord     flags key      ()
  SendText_        s              -> SendText         s              ()
  SendMouseClick_  flags n button -> SendMouseClick   flags n button ()
  SendMouseScroll_ flags scroll n -> SendMouseScroll  flags scroll n ()
  SetClipboard_    s              -> SetClipboard     s              ()
  OpenApplication_ app            -> OpenApplication  app            ()
  OpenURL_         url            -> OpenURL          url            ()
  Delay_           t              -> Delay            t              ()

--------------------------------------------------------------------------------
{-$ CoWorkflowF

provides generic helper functions for defining interpreters.

-}

{-|

a generic handler/interpreter (product type)
for 'WorkflowF' effects (a sum type).

'Delay' is elided, as its implementation can use
cross-platform 'IO' ('threadDelay').

see 'runWorkflowWithT'

Naming: induces a @CoMonad@, see
<http://dlaing.org/cofun/posts/free_and_cofree.html>

'WorkflowF', 'CoWorkflowF', and 'runWorkflowWithT' are analogous to:

* @Either (a -> c) (b,c)@,
"get an @a@, or set a @b@"
* @((a,c), (b -> c))@,
"a handler for the getting of an @a@, and a handler for the setting of a @b@"
* @
handle
 :: ((a,c), (b -> c))
 -> (Either (a -> c) (b,c))
 -> c
handle (aHasBeenGotten, bHasBeenSet) = either TODO
@

background: see
<http://dlaing.org/cofun/posts/free_and_cofree.html>

-}
data CoWorkflowF k = CoWorkflowF
   { _sendKeyChord       :: ([Modifier] -> Key -> k)
   , _sendText           :: (String            -> k)

   , _sendMouseClick     :: ([Modifier] -> Natural     -> MouseButton -> k)
   , _sendMouseScroll    :: ([Modifier] -> MouseScroll -> Natural     -> k)

   , _getClipboard       :: (Clipboard  , k)
   , _setClipboard       :: (Clipboard -> k)

   , _currentApplication :: (Application  , k)
   , _openApplication    :: (Application -> k)

   , _openURL            :: (URL -> k)

   , _delay              :: (MilliSeconds -> k)

   } deriving (Functor)

{-

class (Functor f, Functor g) => Pairing f g where
 pair :: (a -> b -> r) -> (f a -> g b -> r)

instance Pairing CoWorkflowF WorkflowF where
 pair :: (a -> b -> r) -> (CoWorkflowF a -> WorkflowF b -> r)

 pair u CoWorkflowF{..} = \case

  GetClipboard f    -> let (s,a) = _getClipboard in
                       u a (f s)

  SetClipboard s b  -> u (_setClipboard s) b

  ...

-}

{-| an explicit type-class "dictionary" for 'MonadWorkflow'.

e.g.

@
@

expansion:

@
CoWorkflowT w a
~
CofreeT CoWorkflowF w a
~
w (CofreeF CoWorkflowF a (CofreeT CoWorkflowF w a))
~
w (a, CoWorkflowF (CofreeT CoWorkflowF w a))
@

since:

@
data CofreeT f w a = w (CofreeF f a (CofreeT f w a))
@

-}
type CoWorkflowT = CofreeT CoWorkflowF

{-|

expansion:

@
CoWorkflow a
~
Cofree CoWorkflowF a
~
(a, CoWorkflow (Cofree CoWorkflowF a))
@

since:

@
data Cofree f a = a :< f (Cofree f a)
@

-}
type CoWorkflow = Cofree CoWorkflowF

-- [old] naming: WorkflowD

-- {-|
--
-- -}
-- runWorkflowWithT
--   :: forall m a. (MonadIO m)
--   => CoWorkflowT (m a)
--   -> WorkflowT    m a
--   -> m a
-- runWorkflowWithT CoWorkflowF{..} = iterT go
--  where
--
--  go :: WorkflowF (m a) -> m a
--  go = \case
--
--   SendKeyChord    flags key k      -> _sendKeyChord flags key >> k
--   SendText        s k              -> _sendText s             >> k
--
--   SendMouseClick  flags n button k    -> _sendMouseClick flags n button     >> k
--   SendMouseClick  flags scrolling n k -> _sendMouseScroll flags scrolling n >> k
--
--   GetClipboard    f                -> _getClipboard   >>= f
--   SetClipboard    s k              -> _setClipboard s >>  k
--
--   CurrentApplication f             -> _currentApplication  >>= f
--   OpenApplication app k            -> _openApplication app >>  k
--   OpenURL         url k            -> _openURL url         >>  k
--
--   Delay           t k              -> liftIO (threadDelay (t*1000)) >> k
--  -- 1,000 µs is 1ms
