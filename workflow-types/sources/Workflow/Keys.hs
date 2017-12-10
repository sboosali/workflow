{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module Workflow.Keys where --TODO mv to workflow-derived
import Workflow.Extra
import Workflow.Types
import Workflow.Lens

import Data.List.Split

import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)

--------------------------------------------------------------------------------

{-| Parses and executes a keyboard shortcut. (via 'readKeySequence' and 'sendKeyChord').

* more convenient than manually constructing 'Modifier's and 'Key's.
* but, (safely) partial

e.g. compare:

@
press "H-S-t H-l"
@

to:

@
'traverse_' 'sendKeyChord''
 [ 'KeyChord' ['HyperModifier', 'ShiftModifier'] 'TKey'
 , 'KeyChord' ['HyperModifier') 'LKey'
 ]
@

(a keyboard shortcut to "re-open tab, then jump to url bar")

'throwM's on a "syntax error"

The default syntax is inspired by Emacs:

@
press = 'press'' 'defaultKeyChordSyntax'
@

-}
press :: (MonadWorkflow m, MonadThrow m) => String -> m ()
press = press' defaultKeyChordSyntax

{-|

>>> readEmacsKeySequence "H-S-t H-l"
Just [([HyperModifier,ShiftModifier],TKey),([HyperModifier],LKey)]

@='readEmacsKeySequence' 'emacsKeyChordSyntax'@

-}
readEmacsKeySequence :: String -> Maybe KeySequence
readEmacsKeySequence = readKeySequence emacsKeyChordSyntax

{-|

>>> readEmacsKeyChord "H-S-t"
Just ([HyperModifier,ShiftModifier],TKey)

@='readEmacsKeyChord' 'emacsKeyChordSyntax'@

-}
readEmacsKeyChord :: String -> Maybe KeySequence
readEmacsKeyChord = readKeySequence emacsKeyChordSyntax

{- |

>>> readEmacsModifier "H"
Just HyperModifier

@='readModifier' 'emacsModifierSyntax'@

-}
readEmacsModifier :: String -> Maybe Modifier
readEmacsModifier = readModifier emacsModifierSyntax

{- |

>>> readEmacsKey "<tab>"
Just TabKey

@='readKey' 'emacsKeySyntax'@

-}
readEmacsKey :: String -> Maybe KeyChord
readEmacsKey = readKey emacsKeySyntax

--------------------------------------------------------------------------------

{-| Build your own 'press'. e.g.

@
import "Workflow.Core" hiding (press)

press = press' KeyChordSyntax{..}

modifierSyntax :: ModifierSyntax
modifierSyntax = defaultModifierSyntax -- defaulting

keySyntax :: KeySyntax     -- overriding
keySyntax = Map.fromList
 [ ...
 ]

@
-}
press' :: (MonadWorkflow m, MonadThrow m) => KeyChordSyntax -> String -> m ()
press' syntax s = go s

  where
  go = (readKeySequence syntax >>> __cast__) >=> traverse_ sendKeyChord'
  __cast__ = \case
    Nothing -> __fail__
    Just [] -> __fail__ --TODO? readKeySequence :: MonadThrow m => String -> m KeySequence
    Just ks -> return ks --TODO NonEmpty

  __fail__ = failed $ "syntax error: {{Workflow.Keys.press "++(show s)++"}}"
    -- TODO? Control.Monad.Fail (MonadFail(..))

readKeySequence :: KeyChordSyntax -> String -> Maybe KeySequence --TODO Either ReadKeySequenceError / ErrorReadingKeySequence
readKeySequence syntax --TODO plural? --TODO make extensible with Map Char Key and Map String Modifier and inner-div and outer-div
 = splitOn " " >>> fmap (splitOn "-") >>> traverse (readKeyChord syntax)
 -- >>> fmap concat

readKeyChord :: KeyChordSyntax -> [String] -> Maybe KeyChord
readKeyChord KeyChordSyntax{..} = \case
 []      -> Nothing
 s@(_:_) -> do
   ms <- traverse (readModifier modifierSyntax) (init s) --NOTE total, TODO prove with NonEmpty.init
   k  <-          (readKey      keySyntax)      (last s) --NOTE total --TODO munch uppercase until lwoer
   return $ addMods ms k

-- | surjective, non-injective.
readModifier :: ModifierSyntax -> String -> Maybe Modifier
readModifier = (flip Map.lookup)

-- |
readKey :: KeySyntax -> String -> Maybe KeyChord --TODO string for non-alphanums like <spc>
readKey = (flip Map.lookup)

--------------------------------------------------------------------------------

{-| A table for parsing strings of modifiers and keys.

-}
data KeyChordSyntax = KeyChordSyntax
 { modifierSyntax :: ModifierSyntax
 , keySyntax      :: KeySyntax
 }  deriving (Show,Read,Eq,Ord,Data,Generic)
instance NFData KeyChordSyntax

-- | '<>' overrides (i.e. right-biased i.e. pick the last).
instance Monoid KeyChordSyntax where
  mempty = KeyChordSyntax mempty mempty
  mappend (KeyChordSyntax m1 k1) (KeyChordSyntax m2 k2) = KeyChordSyntax{..}
   where
   modifierSyntax = Map.unionWith (curry snd) m1 m2
   keySyntax      = Map.unionWith (curry snd) k1 k2

type ModifierSyntax = Map String Modifier

type KeySyntax = Map String KeyChord

-- | @= 'emacsKeyChordSyntax'@
defaultKeyChordSyntax :: KeyChordSyntax
defaultKeyChordSyntax = KeyChordSyntax defaultModifierSyntax defaultKeySyntax

-- | @= 'emacsModifierSyntax'@
defaultModifierSyntax :: ModifierSyntax
defaultModifierSyntax = emacsModifierSyntax

-- | @= 'emacsKeySyntax'@
defaultKeySyntax :: KeySyntax
defaultKeySyntax = emacsKeySyntax

-- | @= 'KeyChordSyntax' 'defaultModifierSyntax' 'defaultKeySyntax'@
emacsKeyChordSyntax :: KeyChordSyntax
emacsKeyChordSyntax = KeyChordSyntax defaultModifierSyntax defaultKeySyntax

-- | (see source)
emacsModifierSyntax :: ModifierSyntax
emacsModifierSyntax = Map.fromList
  [ "M" -: MetaModifier
  , "H" -: HyperModifier --TODO C
  , "C" -: ControlModifier --TODO N / R / L
  , "O" -: OptionModifier --TODO A
  , "A" -: OptionModifier
  , "S" -: ShiftModifier
  , "F" -: FunctionModifier
  ]

{- |

follows <http://emacswiki.org/emacs/EmacsKeyNotation Emacs keybinding syntax>, with some differences:

* non-modifier uppercase alphabetic characters are not shifted, for consistency:

      * e.g. use @"M-S-a"@, not @"M-A"@
      * e.g. but @"M-:"@ and @"M-S-;"@ can both be used

* non-alphanumeric characters can be in angle brackets:

      * e.g. use @"C-<tab>"@, not @"C-TAB"@
      * e.g. but @"C-\t"@ can be used

(see source)

-}
emacsKeySyntax :: KeySyntax --TODO newtype, def, IsList --TODO modules, neither explicit nor implicit param
emacsKeySyntax = Map.fromList
  [ "a"  -: KeyChord [             ] AKey
  , "A"  -: KeyChord [ShiftModifier] AKey
  , "b"  -: KeyChord [             ] BKey
  , "B"  -: KeyChord [ShiftModifier] BKey
  , "c"  -: KeyChord [             ] CKey
  , "C"  -: KeyChord [ShiftModifier] CKey
  , "d"  -: KeyChord [             ] DKey
  , "D"  -: KeyChord [ShiftModifier] DKey
  , "e"  -: KeyChord [             ] EKey
  , "E"  -: KeyChord [ShiftModifier] EKey
  , "f"  -: KeyChord [             ] FKey
  , "F"  -: KeyChord [ShiftModifier] FKey
  , "g"  -: KeyChord [             ] GKey
  , "G"  -: KeyChord [ShiftModifier] GKey
  , "h"  -: KeyChord [             ] HKey
  , "H"  -: KeyChord [ShiftModifier] HKey
  , "i"  -: KeyChord [             ] IKey
  , "I"  -: KeyChord [ShiftModifier] IKey
  , "j"  -: KeyChord [             ] JKey
  , "J"  -: KeyChord [ShiftModifier] JKey
  , "k"  -: KeyChord [             ] KKey
  , "K"  -: KeyChord [ShiftModifier] KKey
  , "l"  -: KeyChord [             ] LKey
  , "L"  -: KeyChord [ShiftModifier] LKey
  , "m"  -: KeyChord [             ] MKey
  , "M"  -: KeyChord [ShiftModifier] MKey
  , "n"  -: KeyChord [             ] NKey
  , "N"  -: KeyChord [ShiftModifier] NKey
  , "o"  -: KeyChord [             ] OKey
  , "O"  -: KeyChord [ShiftModifier] OKey
  , "p"  -: KeyChord [             ] PKey
  , "P"  -: KeyChord [ShiftModifier] PKey
  , "q"  -: KeyChord [             ] QKey
  , "Q"  -: KeyChord [ShiftModifier] QKey
  , "r"  -: KeyChord [             ] RKey
  , "R"  -: KeyChord [ShiftModifier] RKey
  , "s"  -: KeyChord [             ] SKey
  , "S"  -: KeyChord [ShiftModifier] SKey
  , "t"  -: KeyChord [             ] TKey
  , "T"  -: KeyChord [ShiftModifier] TKey
  , "u"  -: KeyChord [             ] UKey
  , "U"  -: KeyChord [ShiftModifier] UKey
  , "v"  -: KeyChord [             ] VKey
  , "V"  -: KeyChord [ShiftModifier] VKey
  , "w"  -: KeyChord [             ] WKey
  , "W"  -: KeyChord [ShiftModifier] WKey
  , "x"  -: KeyChord [             ] XKey
  , "X"  -: KeyChord [ShiftModifier] XKey
  , "y"  -: KeyChord [             ] YKey
  , "Y"  -: KeyChord [ShiftModifier] YKey
  , "z"  -: KeyChord [             ] ZKey
  , "Z"  -: KeyChord [ShiftModifier] ZKey

  , "0"  -: KeyChord [             ] ZeroKey
  , ")"  -: KeyChord [ShiftModifier] ZeroKey
  , "1"  -: KeyChord [             ] OneKey
  , "!"  -: KeyChord [ShiftModifier] OneKey
  , "2"  -: KeyChord [             ] TwoKey
  , "@"  -: KeyChord [ShiftModifier] TwoKey
  , "3"  -: KeyChord [             ] ThreeKey
  , "#"  -: KeyChord [ShiftModifier] ThreeKey
  , "4"  -: KeyChord [             ] FourKey
  , "$"  -: KeyChord [ShiftModifier] FourKey
  , "5"  -: KeyChord [             ] FiveKey
  , "%"  -: KeyChord [ShiftModifier] FiveKey
  , "6"  -: KeyChord [             ] SixKey
  , "^"  -: KeyChord [ShiftModifier] SixKey
  , "7"  -: KeyChord [             ] SevenKey
  , "&"  -: KeyChord [ShiftModifier] SevenKey
  , "8"  -: KeyChord [             ] EightKey
  , "*"  -: KeyChord [ShiftModifier] EightKey
  , "9"  -: KeyChord [             ] NineKey
  , "("  -: KeyChord [ShiftModifier] NineKey

  , "`"  -: KeyChord [             ] GraveKey
  , "~"  -: KeyChord [ShiftModifier] GraveKey
  , "<dash>"  -: KeyChord [             ] MinusKey 
  , "-"  -: KeyChord [             ] MinusKey --TODO fails
  , "_"  -: KeyChord [ShiftModifier] MinusKey
  , "="  -: KeyChord [             ] EqualKey
  , "+"  -: KeyChord [ShiftModifier] EqualKey
  , "["  -: KeyChord [             ] LeftBracketKey
  , "{"  -: KeyChord [ShiftModifier] LeftBracketKey
  , "]"  -: KeyChord [             ] RightBracketKey
  , "}"  -: KeyChord [ShiftModifier] RightBracketKey
  , "\\" -: KeyChord [             ] BackslashKey
  , "|"  -: KeyChord [ShiftModifier] BackslashKey
  , ";"  -: KeyChord [             ] SemicolonKey
  , ":"  -: KeyChord [ShiftModifier] SemicolonKey
  , "'"  -: KeyChord [             ] QuoteKey
  , "\"" -: KeyChord [ShiftModifier] QuoteKey
  , ","  -: KeyChord [             ] CommaKey
  , "<"  -: KeyChord [ShiftModifier] CommaKey
  , "."  -: KeyChord [             ] PeriodKey
  , ">"  -: KeyChord [ShiftModifier] PeriodKey
  , "/"  -: KeyChord [             ] SlashKey
  , "?"  -: KeyChord [ShiftModifier] SlashKey
  , " "  -: KeyChord [             ] SpaceKey
  , "\t" -: KeyChord [             ] TabKey
  , "\n" -: KeyChord [             ] ReturnKey

  , "<spc>"-: SimpleKeyChord SpaceKey
  , "<tab>"-: SimpleKeyChord TabKey
  , "<ret>"-: SimpleKeyChord ReturnKey
  , "<del>"-: SimpleKeyChord DeleteKey
  , "<esc>"-: SimpleKeyChord EscapeKey

  , "<up>"   -: SimpleKeyChord UpArrowKey
  , "<down>" -: SimpleKeyChord DownArrowKey
  , "<left>" -: SimpleKeyChord LeftArrowKey
  , "<right>"-: SimpleKeyChord RightArrowKey

  , "<f1>" -: SimpleKeyChord F1Key
  , "<f2>" -: SimpleKeyChord F2Key
  , "<f3>" -: SimpleKeyChord F3Key
  , "<f4>" -: SimpleKeyChord F4Key
  , "<f5>" -: SimpleKeyChord F5Key
  , "<f6>" -: SimpleKeyChord F6Key
  , "<f7>" -: SimpleKeyChord F7Key
  , "<f8>" -: SimpleKeyChord F8Key
  , "<f9>" -: SimpleKeyChord F9Key
  , "<f10>"-: SimpleKeyChord F10Key
  , "<f11>"-: SimpleKeyChord F11Key
  , "<f12>"-: SimpleKeyChord F12Key
  , "<f13>"-: SimpleKeyChord F13Key
  , "<f14>"-: SimpleKeyChord F14Key
  , "<f15>"-: SimpleKeyChord F15Key
  , "<f16>"-: SimpleKeyChord F16Key
  , "<f17>"-: SimpleKeyChord F17Key
  , "<f18>"-: SimpleKeyChord F18Key
  , "<f19>"-: SimpleKeyChord F19Key
  , "<f20>"-: SimpleKeyChord F20Key
  ]

--------------------------------------------------------------------------------

-- | appends modifiers
addMods :: [Modifier] -> KeyChord -> KeyChord
addMods ms kc = foldr addMod kc ms

-- | appends a modifier
addMod :: Modifier -> KeyChord -> KeyChord
addMod m (ms, k) = (m:ms, k)
-- false positive nonexhaustive warning with the KeyChord pattern. fixed in ghc8?
-- addMod m (KeyChord ms k) = KeyChord (m:ms) k

--------------------------------------------------------------------------------

{-TODO

Is The benefit of avoiding a parser library as a dependency worth the awkwardness?

-}

{- the keychord that would insert the character into the application.

>>> char2keychord '@' :: Maybe KeyChord
Just ([ShiftModifier], TwoKey)

some characters cannot be represented as keypresses, like some non-printable characters
(in arbitrary applications, not just the terminal emulator):

>>> char2keychord '\0' :: Maybe KeyChord
Nothing

prop> case char2keychord c of {  Just ([],_) -> True;  Just ([ShiftModifier],_) -> True;  Nothing -> True;  _ -> False  }

-}
char2keychord :: (MonadThrow m) => Char -> m KeyChord
char2keychord c = case c of

 'a'  -> return $ KeyChord [             ] AKey
 'A'  -> return $ KeyChord [ShiftModifier] AKey
 'b'  -> return $ KeyChord [             ] BKey
 'B'  -> return $ KeyChord [ShiftModifier] BKey
 'c'  -> return $ KeyChord [             ] CKey
 'C'  -> return $ KeyChord [ShiftModifier] CKey
 'd'  -> return $ KeyChord [             ] DKey
 'D'  -> return $ KeyChord [ShiftModifier] DKey
 'e'  -> return $ KeyChord [             ] EKey
 'E'  -> return $ KeyChord [ShiftModifier] EKey
 'f'  -> return $ KeyChord [             ] FKey
 'F'  -> return $ KeyChord [ShiftModifier] FKey
 'g'  -> return $ KeyChord [             ] GKey
 'G'  -> return $ KeyChord [ShiftModifier] GKey
 'h'  -> return $ KeyChord [             ] HKey
 'H'  -> return $ KeyChord [ShiftModifier] HKey
 'i'  -> return $ KeyChord [             ] IKey
 'I'  -> return $ KeyChord [ShiftModifier] IKey
 'j'  -> return $ KeyChord [             ] JKey
 'J'  -> return $ KeyChord [ShiftModifier] JKey
 'k'  -> return $ KeyChord [             ] KKey
 'K'  -> return $ KeyChord [ShiftModifier] KKey
 'l'  -> return $ KeyChord [             ] LKey
 'L'  -> return $ KeyChord [ShiftModifier] LKey
 'm'  -> return $ KeyChord [             ] MKey
 'M'  -> return $ KeyChord [ShiftModifier] MKey
 'n'  -> return $ KeyChord [             ] NKey
 'N'  -> return $ KeyChord [ShiftModifier] NKey
 'o'  -> return $ KeyChord [             ] OKey
 'O'  -> return $ KeyChord [ShiftModifier] OKey
 'p'  -> return $ KeyChord [             ] PKey
 'P'  -> return $ KeyChord [ShiftModifier] PKey
 'q'  -> return $ KeyChord [             ] QKey
 'Q'  -> return $ KeyChord [ShiftModifier] QKey
 'r'  -> return $ KeyChord [             ] RKey
 'R'  -> return $ KeyChord [ShiftModifier] RKey
 's'  -> return $ KeyChord [             ] SKey
 'S'  -> return $ KeyChord [ShiftModifier] SKey
 't'  -> return $ KeyChord [             ] TKey
 'T'  -> return $ KeyChord [ShiftModifier] TKey
 'u'  -> return $ KeyChord [             ] UKey
 'U'  -> return $ KeyChord [ShiftModifier] UKey
 'v'  -> return $ KeyChord [             ] VKey
 'V'  -> return $ KeyChord [ShiftModifier] VKey
 'w'  -> return $ KeyChord [             ] WKey
 'W'  -> return $ KeyChord [ShiftModifier] WKey
 'x'  -> return $ KeyChord [             ] XKey
 'X'  -> return $ KeyChord [ShiftModifier] XKey
 'y'  -> return $ KeyChord [             ] YKey
 'Y'  -> return $ KeyChord [ShiftModifier] YKey
 'z'  -> return $ KeyChord [             ] ZKey
 'Z'  -> return $ KeyChord [ShiftModifier] ZKey

 '0'  -> return $ KeyChord [             ] ZeroKey
 ')'  -> return $ KeyChord [ShiftModifier] ZeroKey
 '1'  -> return $ KeyChord [             ] OneKey
 '!'  -> return $ KeyChord [ShiftModifier] OneKey
 '2'  -> return $ KeyChord [             ] TwoKey
 '@'  -> return $ KeyChord [ShiftModifier] TwoKey
 '3'  -> return $ KeyChord [             ] ThreeKey
 '#'  -> return $ KeyChord [ShiftModifier] ThreeKey
 '4'  -> return $ KeyChord [             ] FourKey
 '$'  -> return $ KeyChord [ShiftModifier] FourKey
 '5'  -> return $ KeyChord [             ] FiveKey
 '%'  -> return $ KeyChord [ShiftModifier] FiveKey
 '6'  -> return $ KeyChord [             ] SixKey
 '^'  -> return $ KeyChord [ShiftModifier] SixKey
 '7'  -> return $ KeyChord [             ] SevenKey
 '&'  -> return $ KeyChord [ShiftModifier] SevenKey
 '8'  -> return $ KeyChord [             ] EightKey
 '*'  -> return $ KeyChord [ShiftModifier] EightKey
 '9'  -> return $ KeyChord [             ] NineKey
 '('  -> return $ KeyChord [ShiftModifier] NineKey

 '`'  -> return $ KeyChord [             ] GraveKey
 '~'  -> return $ KeyChord [ShiftModifier] GraveKey
 '-'  -> return $ KeyChord [             ] MinusKey
 '_'  -> return $ KeyChord [ShiftModifier] MinusKey
 '='  -> return $ KeyChord [             ] EqualKey
 '+'  -> return $ KeyChord [ShiftModifier] EqualKey
 '['  -> return $ KeyChord [             ] LeftBracketKey
 '{'  -> return $ KeyChord [ShiftModifier] LeftBracketKey
 ']'  -> return $ KeyChord [             ] RightBracketKey
 '}'  -> return $ KeyChord [ShiftModifier] RightBracketKey
 '\\' -> return $ KeyChord [             ] BackslashKey
 '|'  -> return $ KeyChord [ShiftModifier] BackslashKey
 ';'  -> return $ KeyChord [             ] SemicolonKey
 ':'  -> return $ KeyChord [ShiftModifier] SemicolonKey
 '\'' -> return $ KeyChord [             ] QuoteKey
 '"'  -> return $ KeyChord [ShiftModifier] QuoteKey
 ','  -> return $ KeyChord [             ] CommaKey
 '<'  -> return $ KeyChord [ShiftModifier] CommaKey
 '.'  -> return $ KeyChord [             ] PeriodKey
 '>'  -> return $ KeyChord [ShiftModifier] PeriodKey
 '/'  -> return $ KeyChord [             ] SlashKey
 '?'  -> return $ KeyChord [ShiftModifier] SlashKey
 ' '  -> return $ KeyChord [             ] SpaceKey
 '\t' -> return $ KeyChord [             ] TabKey
 '\n' -> return $ KeyChord [             ] ReturnKey

 _    -> failed $ "{{ char2keychord "++(show c)++" }} not an ASCII, printable character"

