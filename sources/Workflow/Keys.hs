{-| see 'press'.

-}
module Workflow.Keys where
import Workflow.Types
import Workflow.Extra

import Data.List.Split

import qualified Data.Map as Map
import           Data.Map (Map)

--------------------------------------------------------------------------------

{-| Parses and executes a keyboard shortcut. (via 'readKeySequence' and 'sendKeyChord').

* more convenient
but (safely) partial.

The default syntax is inspired by Emacs.

'throwM's on a "syntax error"

-}
press :: (MonadWorkflow m, MonadThrow m) => String -> m ()
press s = go s

  where
  go = (readKeySequence >>> __cast__) >=> traverse_ sendKeyChord'
  __cast__ = maybe
    (failed $ "syntax error: {{Workflow.Keys.press "++(show s)++"}}")
    -- TODO? Control.Monad.Fail (MonadFail(..))
    (return)

{-|

>>> readKeySequence "H-S-t H-l"  -- "re-open tab", then "jump to url bar"
Just [([HyperModifier,ShiftModifier],[TKey]),([HyperModifier],[LKey])]

-}
readKeySequence :: String -> Maybe KeySequence --TODO Either ReadKeySequenceError / ErrorReadingKeySequence
readKeySequence --TODO make extensible with Map Char Key and Map String Modifier and inner-div and outer-div
 = splitOn " " >>> fmap (splitOn "-") >>> traverse readKeyChord
 -- >>> fmap concat

readKeyChord :: [String] -> Maybe KeyChord
readKeyChord = \case
 []      -> Nothing
 s@(_:_) -> do
   ms <- traverse readModifier (init s) --NOTE total, TODO prove with NonEmpty.init
   k  <-          readKey      (last s) --NOTE total --TODO munch uppercase until lwoer
   return $ addMods ms k

-- | surjective, non-injective.
readModifier :: String -> Maybe Modifier
readModifier = (flip Map.lookup) defaultModifierSyntax

-- |
readKey :: String -> Maybe KeyChord --TODO string for non-alphanums like <spc>
readKey = (flip Map.lookup) defaultKeyChordSyntax

--------------------------------------------------------------------------------

-- | (see source)
defaultModifierSyntax :: Map String Modifier
defaultModifierSyntax = Map.fromList
  [ "M" -: MetaModifier
  , "H" -: HyperModifier --TODO C
  , "C" -: ControlModifier --TODO N / R / L
  , "O" -: OptionModifier --TODO A
  , "A" -: OptionModifier
  , "S" -: ShiftModifier
  , "F" -: FunctionModifier
  ]

-- | (see source)
defaultKeyChordSyntax :: Map String KeyChord --TODO newtype, def, IsList --TODO modules, neither explicit nor implicit param
defaultKeyChordSyntax = Map.fromList
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
  , "-"  -: KeyChord [             ] MinusKey
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
  , "'" -: KeyChord [             ] QuoteKey
  , "\""  -: KeyChord [ShiftModifier] QuoteKey
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

{- the keypress that would insert the character into the application.

>>> char2keypress '@' :: Maybe KeyChord
Just ([ShiftModifier], TwoKey)

some characters cannot be represented as keypresses, like some non-printable characters
(in arbitrary applications, not just the terminal emulator):

>>> char2keypress '\0' :: Maybe KeyChord
Nothing

prop> case char2keypress c of {  Just ([],_) -> True;  Just ([ShiftModifier],_) -> True;  Nothing -> True;  _ -> False  }

-}
