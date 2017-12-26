{-# LANGUAGE NoImplicitPrelude, PackageImports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveAnyClass #-}
{-# LANGUAGE PatternSynonyms, DerivingStrategies, DataKinds, UndecidableInstances, GADTs, TypeApplications, RecordWildCards #-}

{-|

-}
module Workflow.Class where

import "enumerate" Enumerate 

import Data.Word (Word)
import Data.String (IsString(..)) 
import GHC.Exts (IsList(..), coerce)
import Control.Monad.Catch

import Data.Hashable (Hashable(..),hashUsing)
import Control.DeepSeq (NFData(..))

import "spiros" Prelude.Spiros
import Data.Semigroup -- Monoid 
import "base"   Data.Ix (Ix(..)) -- TODO
import "base"   Data.Bits (Bits(..),FiniteBits(..)) -- TODO
import "base"   Prelude (Enum(..),error )
import "base"   Data.Functor.Identity
import "base" Data.List.NonEmpty 
import Language.Haskell.TH.Syntax
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Proxy

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

an 'Application' has one-or-more 'Window's.


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

-- press :: KCs -> m ()

--------------------------------------------------------------------------------

  
{-|

-}
newtype RawKeyChord = RawKeyChord Word
 deriving stock   (Show)
 deriving newtype (Eq,Ord,Num,Bits,NFData,Hashable)

{-|

-}
newtype RawKey = RawKey Word
 deriving stock   (Show)
 deriving newtype (Eq,Ord,Num,NFData,Hashable)

{-|

-}
newtype RawModifier = RawModifier Word
 deriving stock   (Show)
 deriving newtype (Eq,Ord,Num,NFData,Hashable)

{-|

-}
newtype RawMouseButton = RawMouseButton Word
 deriving stock   (Show)
 deriving newtype (Eq,Ord,Num,NFData,Hashable)

{-|

-}
newtype RawMouseScroll = RawMouseScroll Word
 deriving stock   (Show)
 deriving newtype (Eq,Ord,Num,NFData,Hashable)

--------------------------------------------------------------------------------

{-

{-

>>> :set -XOverloadedStrings
>>> "contents" :: Clipboard
"contents"

-}
newtype Clipboard_ = Clipboard String
 deriving (Show,Read,Eq,Ord,IsString,{- Data, -}Generic,Semigroup,NFData)
data Clipboard = Clipboard { cbContents :: String, cbFormat :: ClipboardFormat }
instance IsString UnicodeTextFormat where fromString s = Clipboard s UnicodeTextFormat
GetClipboardContents
getClipboard = getClipboardContents<&>cbContents
runWorkflow must take a mapping (ClipboardFormat -> something)
 maybe: phantom data RawClipboard (format :: ClipboardFormat) = Bytestring
 with reflection class KnownClipboardFormat

-}

{- |

@
scheme:[//[user[:password]@]host[:port]][/path][?query][#fragment]
@

-}
newtype URL = URL String
 deriving newtype (Eq,Ord,Show,IsString,Semigroup,NFData,Hashable)

{-|

>>> :set -XOverloadedStrings
>>> "Emacs" :: Application
"Emacs"

-}
newtype Application = Application String
 deriving newtype (Eq,Ord,Show,IsString,NFData,Hashable)

{-|

>>> :set -XOverloadedStrings
>>> "" :: Window 
""

-}
newtype Window = Window String
 deriving newtype (Eq,Ord,Show,IsString,NFData,Hashable)


--------------------------------------------------------------------------------

{-| Operating systems always (?) support at least these mouse events.

Most mice have these three buttons, trackpads have left/right.

-}
data MouseButton
 = LeftButton
 | MiddleButton
 | RightButton
 --TODO | XButton -- https://msdn.microsoft.com/en-us/library/windows/desktop/gg153549(v=vs.85).aspx
 deriving (Show,Read,Eq,Ord,Enum,Bounded,{- Data, -}Generic,NFData,Hashable)


{-| Mouse wheel scrolling, vertically and horizontally.

'ScrollTowards':

* scrolls up when "natural scrolling" is disabled
* scrolls down when "natural scrolling" is enabled

TODO check

-}
data MouseScroll
  = ScrollTowards -- ScrollUp (from user)
  | ScrollAway -- ScrollDown (from user)
  | ScrollLeft
  | ScrollRight
  deriving stock    (Show,Read,Eq,Ord,Enum,Bounded,{- Data, -}Generic,Lift)
  deriving anyclass (NFData,Hashable,Enumerable)

--------------------------------------------------------------------------------


{-| Represents a set of actions @a@, each mapped to one-or-more keyboard shortcuts.

equivalently, a list of 'KeyBinding's, where conflicts are resolved via @f@.
A conflict is when @KeyMapping@s are being merged,
but the same 'KeySequence' is mapped to different @a@'s.

specializations:

@
KeyMapping 'Last'     a
KeyMapping 'First'    a
KeyMapping 'Identity' a
@ 

for example, to resolve conflicts by shadowing use 'Last',
which keeps the rightmost\/last action:

@
KeyMapping 'Last'
@

e.g. 

@
defaultKeyMapping :: KeyMapping Last (Workflow ()) 
customKeyMapping  :: KeyMapping Last (Workflow ())

defaultKeyMapping <> customKeyMapping
-- if they share a KeySequence, we resolve them via 'Last', 
-- whose @Semigroup@ instance is @(_ <> r = r)@.
@

if the "action type" @a@ is itself a @Semigroup@, you can
store them "directly" with 'Identity',
whose behavior is to merge the @a@s via @(<>)@:

@
defaultKeyMapping :: KeyMapping Identity (Workflow ()) 
customKeyMapping  :: KeyMapping Identity (Workflow ())

defaultKeyMapping <> customKeyMapping
-- if they share a KeySequence, we resolve them via the 'Monad' methods, 
-- since @Workflow@'s @Semigroup@ instance is @(l <> r = l '>>' r)@.
@


-}
newtype KeyMapping f a = KeyMapping (Map KeySequence (f a)) 
 deriving stock   (Show, Read, Data) 
 deriving newtype (Eq,Ord,{- Data, -}IsList,NFData)  -- no Generic: Maps are not generic, they are abstract 
-- deriving (Show,Read,Eq,Ord,{- Data, -}NFData,Hashable)

-- | NOTE @f@ determines the merging strategy. the default @Map@ instance uses a left-biased merge, equivalent to @('First' v)@
instance (Semigroup (f a)) => Semigroup (KeyMapping f a) where
  (<>) = todo

instance (Semigroup (f a)) => Monoid (KeyMapping f a) where
  mempty = KeyMapping mempty 

queryLastKeyBinding :: KeyMapping Last a -> KeySequence -> Maybe (KeyBinding a)
queryLastKeyBinding = queryKeyBindingWith getLast 

-- queryMonoidalKeyBinding :: (Monoid a) => KeyMapping Identity a -> KeySequence -> KeyBinding a 
-- queryMonoidalKeyBinding km ks = queryKeyBindingWith runIdentity km ks & maybe mempty id  -- what is the empty keychord? 

queryKeyBindingWith :: (f a -> a) -> KeyMapping f a -> KeySequence -> Maybe (KeyBinding a)
queryKeyBindingWith extract km ks = queryKeyBinding km ks & (fmap.fmap) extract
{-# SPECIALIZE queryKeyBindingWith :: (Last     a -> a) -> KeyMapping Last     a -> KeySequence -> Maybe (KeyBinding a) #-}
{-# SPECIALIZE queryKeyBindingWith :: (First    a -> a) -> KeyMapping First    a -> KeySequence -> Maybe (KeyBinding a) #-}
{-# SPECIALIZE queryKeyBindingWith :: (Identity a -> a) -> KeyMapping Identity a -> KeySequence -> Maybe (KeyBinding a) #-}

queryKeyBinding :: KeyMapping f a -> KeySequence -> Maybe (KeyBinding (f a)) 
queryKeyBinding (KeyMapping bindings) k = KeyBinding k <$> v
  where
  v = Map.lookup k bindings 
{-# SPECIALIZE queryKeyBinding :: KeyMapping Identity (IO()) -> KeySequence -> Maybe (KeyBinding (Identity (IO()))) #-} 

-- queryKeyBinding :: (Comonad f, Monoid a) => KeyMapping f a -> KeySequence -> KeyBinding a 
-- queryKeyBinding (KeyMapping bindings) k = KeyBinding k v 
--   where
--   v = Map.findWith mempty k bindings & extract

-- queryMonoidalKeyBinding :: (Comonad f, Monoid a) => KeyMapping Identity a -> KeySequence -> KeyBinding a 
-- queryMonoidalKeyBinding (KeyMapping bindings) k = KeyBinding k v
--   where
--   v Map.findWith mempty k bindings & runIdentity 

{-| Represents @a@ being bound to a keyboard shortcut.

Naming:
<https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Commands.html>

-}
data KeyBinding a = KeyBinding KeySequence a
 deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable)
 deriving (Functor,Foldable,Traversable)
 -- TODO Applicative? like Const, merging the key sequences somehow, either append or first/last (should still be associative, since First/Last are Monoids) 

{-| a sequence of key chords make up a keyboard shortcut

Naming:
<https://www.emacswiki.org/emacs/KeySequence>

-}
newtype KeySequence = KeySequence [KeyChord]
 deriving stock   (Show, Read, Data, Generic) 
 deriving newtype (Eq,Ord,IsList,Semigroup,Monoid,NFData,Hashable)

instance IsString KeySequence where
  fromString = readKeySequence

readKeySequence :: String -> KeySequence
readKeySequence = todo
 
{- | represents jointly\/simultaneously holding down the 'kcModifiers'
while pressing the 'kcKey' down and back up.

TODO a sequence, unordered and without duplicates,
of keypresses make up a keychord.

Naming: https://www.emacswiki.org/emacs/Chord

-}
data KeyChord = KeyChord
 { kcModifiers :: Modifiers -- (Set Modifier) -- [Modifier]
 , kcKey       :: Key
 } deriving (Show,Read,Eq,Ord,Data) -- ,Generic) -- ,Hashable)

instance NFData KeyChord where
  rnf KeyChord{..} = rnf kcModifiers `seq` rnf kcKey `seq` ()

instance Hashable KeyChord where
  hashWithSalt = hashUsing tupleKeyChord

instance Enumerable KeyChord where
  enumerated = KeyChord <$> enumerated @Modifiers <*> enumerated @Key -- cross-product
  cardinality _ = cardinality (Proxy @Modifiers) * cardinality (Proxy @Key) -- product
   -- TODO is there some fancy way to do this, like the Applicative of Const Product, so I don't mess it up if I re-factor it
  -- = KeyChord <$> cardinality (Proxy @Modifiers) * cardinality (Proxy @
  
-- data KeyChord = KeyChord (Set Modifier) (NonEmpty Key) 

tupleKeyChord :: KeyChord -> (Modifiers, Key)
tupleKeyChord KeyChord{..} = (kcModifiers,kcKey)

readKeyChord :: String -> KeyChord
readKeyChord = todo

unmodifiedKeyChord :: Key -> KeyChord
unmodifiedKeyChord = KeyChord mempty -- [k]

-- pattern SimpleKeyChord :: Key -> KeyChord
-- pattern SimpleKeyChord k = KeyChord Set.empty [k]   -- not a pattern 

{-|

(really, a @Set@)

TODO are they unordered? no duplicates but with order.

-}
newtype Modifiers = Modifiers { getModifiers :: Set Modifier } 
 deriving stock   (Data)
 deriving newtype (Show,Read) -- this ignores the accessor 
 deriving newtype (Eq,Ord,IsList,Semigroup,Monoid,NFData) -- ,Generic) Maps are not generic, they are abstract 
-- instance Enumerable Modifiers where 

instance Hashable Modifiers where
  -- hashWithSalt i (Modifiers ms) =
  hashWithSalt = hashUsing (getModifiers > Set.toList) 

instance Enumerable Modifiers where
  enumerated = coerce (enumerated @(Set Modifier))
  cardinality _ = cardinality (Proxy @(Set Modifier)) 

instance IsString Modifiers where
  fromString = readModifiers

readModifiers :: String -> Modifiers
readModifiers = todo

{-|

e.g. @[XOptionModifier,XControlModifier,OptionModifier,ControlModifier]@ is equivalent to
@[OptionModifier,ControlModifier]@, on any platform. 

-}
normalizeModifiers :: Modifiers -> Modifiers
normalizeModifiers = todo -- the fake keys

{-

       The type family application `Item [Modifier]'
          is no smaller than the instance head
        (Use UndecidableInstances to permit this)

-}

{- | platform-independent modifier keys.

modifier keys are keys that can be "held".

NOTE the escape key tends to be "pressed", not "held", it seems.
(possibly explains its behavior in your terminal emulator?)

@alt@ is 'OptionModifier', c.f. 'AltModifier.

-}
data Modifier
 = XOptionModifier  -- ^ fake modifier: Alt on Linux\/Windows, Command on OSX
 | XControlModifier -- ^ fake modifier: Control on Linux\/Windows, Command on OSX
 | ControlModifier
 | OptionModifier --TODO rn Option Alt
 | ShiftModifier
 -- TODO | FunctionModifier -- I don't think this is an actual modifier, the modification is at the hardware level 
 deriving (Show,Read,Eq,Ord,Bounded,Enum,Data,Generic,NFData,Hashable,Enumerable)

pattern AltModifier :: Modifier 
pattern AltModifier = OptionModifier

{- | platform-dependent modifier keys. 

-}
data PlatformModifier
 = ControlModifier'
 | OptionModifier'
 | ShiftModifier'
 | CommandModifier' 
 deriving (Show,Read,Eq,Ord,Bounded,Enum,Data,Generic,NFData,Hashable,Enumerable)


{- | all modifier keys, across every major platform. 


reifyModifier :: platform SomeModifier -> SomeModifier


-}
data SomeModifier
 = TheControlModifier
 | TheOptionModifier
 | TheShiftModifier
 | TheCommandModifier
 | TheWindowsModifier
 deriving (Show,Read,Eq,Ord,Bounded,Enum,Data,Generic,NFData,Hashable,Enumerable)

{-| 

-}
data LinuxModifier (modifier :: SomeModifier) where
 LinuxControlModifier :: LinuxModifier 'TheControlModifier
 LinuxOptionModifier  :: LinuxModifier 'TheOptionModifier 
 LinuxShiftModifier   :: LinuxModifier 'TheShiftModifier
 -- deriving (Show,Read,Eq,Ord,Bounded,Enum,Data,Generic,NFData,Hashable,Enumerable)

-- data PlatformModifier
--  = LinuxModifier   LinuxModifier
--  | WindowsModifier LinuxModifier -- TODO the win-logo key; it's a mod, but not in many shortcuts anywyas
--  | MacModifier     MacModifier
--  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable,Enumerable)

-- data LinuxModifier
--  = LinuxControlModifier
--  | LinuxOptionModifier 
--  | LinuxShiftModifier
--  deriving (Show,Read,Eq,Ord,Bounded,Enum,Data,Generic,NFData,Hashable,Enumerable)

-- data LinuxModifier
--  = LinuxControlModifier
--  | LinuxOptionModifier 
--  | LinuxShiftModifier
--  | LinuxWindowsModifier
--  deriving (Show,Read,Eq,Ord,Bounded,Enum,Data,Generic,NFData,Hashable,Enumerable)

-- data MacModifier
--  = MacControlModifier
--  | MacOptionModifier
--  | MacShiftModifier
--  | MacCommandModifier
--  deriving (Show,Read,Eq,Ord,Bounded,Enum,Data,Generic,NFData,Hashable,Enumerable)

fromPlatformModifier :: PlatformModifier -> Maybe Modifier
fromPlatformModifier = \case 
 ControlModifier' -> Just ControlModifier
 OptionModifier'  -> Just OptionModifier
 ShiftModifier'   -> Just ShiftModifier
 CommandModifier' -> Nothing

evaluateModifier :: KnownPlatform -> Modifier -> PlatformModifier
evaluateModifier platform = \case 
 XControlModifier -> evaluateXControlModifier platform
 XOptionModifier  -> evaluateXOptionModifier  platform
 ControlModifier  -> ControlModifier'
 OptionModifier   -> OptionModifier'
 ShiftModifier    -> ShiftModifier'
 where
 evaluateXControlModifier = \case
   LinuxPlatform   -> ControlModifier'
   WindowsPlatform -> ControlModifier'
   MacPlatform     -> CommandModifier' 
 evaluateXOptionModifier = \case
   LinuxPlatform   -> OptionModifier'
   WindowsPlatform -> OptionModifier'
   MacPlatform     -> CommandModifier' 

data KnownPlatform 
   = LinuxPlatform   
   | WindowsPlatform 
   | MacPlatform     
 deriving (Show,Read,Eq,Ord,Bounded,Enum,Data,Generic,NFData,Hashable,Enumerable)

--------------------------------------------------------------------------------

{-| a single-digit decimal number, i.e. @[0..9]@ i.e. @Fin 10@.

the @Num@ instance provides numerical literals, and is partial like @Natural@. 

-}
newtype Digit = Digit Int -- Word8 Natural
 deriving stock   (Show,Read,Data)
 deriving newtype (Eq,Ord,Num,Ix,Bits,FiniteBits,NFData,Hashable) -- no Generic: it's abstract 
 -- TODO are the two bits classes correct ? since they use Int's instance 

instance Bounded Digit where
  minBound = 0
  maxBound = 9

-- | 'toEnum' is partial 
instance Enum Digit where
  toEnum   = Digit -- toInteger > fromInteger > Digit
  fromEnum = digit2int

instance Enumerable Digit where
  enumerated = [0..9]
  cardinality _ = 10

instance Finite Digit where
  type Cardinality Digit = 10

digit2int :: Digit -> Int
digit2int (Digit d) = d

-- TODO toDigit 

{- | a "cross-platform" keyboard, that has:

* all keys that exist on standard keyboards.
* plus, 'XOptionKey' and 'XControlKey': virtual modifiers to abstract over
common keyboard shortcuts.

(let me know if you want a type to support cross-platform international keyboards,
i haven't looked into it. you can still use the
platform-specific virtual-key-codes in the dependent packages:
@workflow-linux@, @workflow-osx@, and @workflow-windows@)

mnemonic: "XControlKey" is "x-platform control" or "osx control" 

-}
data Key

 = XOptionKey -- ^ fake key: Alt on Linux\/Windows, Command on OSX
 | XControlKey -- ^ fake key: Control on Linux\/Windows, Command on OSX
-- Control\/Command both have C\/O\/N
 -- TODO remove these fake keys ? or change the representation, like with a Boolean ;or a Either of "regular versus platform-specific" 

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

 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,Data,NFData,Hashable,Enumerable)

--------------------------------------------------------------------------------

-- | All modifiers are keys.
modifier2key :: Modifier -> Key
modifier2key = \case
 XOptionModifier          -> XOptionKey
 XControlModifier         -> XControlKey
 
 ShiftModifier         -> ShiftKey
 OptionModifier        -> OptionKey
 ControlModifier       -> ControlKey
--  FunctionModifier      -> FunctionKey

-- | Some keys are modifiers. 
isModifierKey :: Key -> Maybe Modifier
isModifierKey = \case
 XOptionKey          -> Just XOptionModifier
 XControlKey         -> Just XControlModifier
 
 ShiftKey         -> Just ShiftModifier
 OptionKey        -> Just OptionModifier
 ControlKey       -> Just ControlModifier
--  FunctionKey      -> Just FunctionModifier
 
 _                -> Nothing

isAlphaNumKey :: Key -> Maybe Char 
isAlphaNumKey x
    =                 isAlphabeticKey x
  <|> (digit2char <$> isNumericKey    x)

digit2char :: Digit -> Char
digit2char = \case
  0 -> '0'
  1 -> '1'
  2 -> '2'
  3 -> '3'
  4 -> '4'
  5 -> '5'
  6 -> '6'
  7 -> '7'
  8 -> '8'
  9 -> '9'
  d -> error $ "[digit2char] " ++ show d  --  __BUG__

isAlphabeticKey :: Key -> Maybe Char
isAlphabeticKey = \case
 AKey -> Just 'a'
 BKey -> Just 'b'
 CKey -> Just 'c'
 DKey -> Just 'd'
 EKey -> Just 'e'
 FKey -> Just 'f'
 GKey -> Just 'g'
 HKey -> Just 'h'
 IKey -> Just 'i'
 JKey -> Just 'j'
 KKey -> Just 'k'
 LKey -> Just 'l'
 MKey -> Just 'm'
 NKey -> Just 'n'
 OKey -> Just 'o'
 PKey -> Just 'p'
 QKey -> Just 'q'
 RKey -> Just 'r'
 SKey -> Just 's'
 TKey -> Just 't'
 UKey -> Just 'u'
 VKey -> Just 'v'
 WKey -> Just 'w'
 XKey -> Just 'x'
 YKey -> Just 'y'
 ZKey -> Just 'z'
 _    -> Nothing

isNumericKey :: Key -> Maybe Digit 
isNumericKey = \case
 ZeroKey  -> Just 0
 OneKey   -> Just 1
 TwoKey   -> Just 2
 ThreeKey -> Just 3
 FourKey  -> Just 4
 FiveKey  -> Just 5
 SixKey   -> Just 6
 SevenKey -> Just 7
 EightKey -> Just 8
 NineKey  -> Just 9
 _        -> Nothing 

-- displayKey :: Key -> String
-- displayKey = show >>> (T.pack >>> T.stripSuffix "Key" >>> maybe (error "workflow-types:Workflow.Types.displayKey") id >>> T.unpack) >>> fmap toLower -- NOTE partial

--------------------------------------------------------------------------------

