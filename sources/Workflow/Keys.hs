module Workflow.Keys where
import Workflow.Types
import Workflow.Extra

import Data.List.Split

import qualified Data.Map as Map
-- import           Data.Map (Map)


{-|

>>> readKeyBinding "H-S-t"  -- "re-open tab"
Just [([HyperModifier,ShiftModifier],[TKey])]

-}
readKeyBinding :: String -> Maybe KeyBinding --TODO Either ReadKeyBindingError / ErrorReadingKeyBinding
readKeyBinding --TODO make extensible with Map Char Key and Map String Modifier and inner-div and outer-div
 = splitOn " " >>> fmap (splitOn "-") >>> traverse readKeyChord

readKeyChord :: [String] -> Maybe KeyChord
readKeyChord = \case
 []      -> Nothing
 s@(_:_) -> (,) <$> ms <*> ks
  where
  ms = traverse readModifier (init s) --NOTE total, TODO prove with NonEmpty.init
  ks = traverse readKey      (last s) --NOTE total

-- | surjective, non-injective.
readModifier :: String -> Maybe Modifier
readModifier = (flip Map.lookup) table
 where
 table = Map.fromList
  [ "M" -: MetaModifier
  , "H" -: HyperModifier --TODO C
  , "C" -: ControlModifier --TODO N / R / L
  , "O" -: OptionModifier --TODO A
  , "A" -: OptionModifier
  , "S" -: ShiftModifier
  , "F" -: FunctionModifier
  ]

-- | non-surjective?
readKey :: Char -> Maybe Key --TODO string for non-alphanums like <spc>
readKey = \case
 't'  -> Just TKey
 _    -> Nothing

  -- <|> NoMod SpaceKey                             <$  E.word "<spc>"
  -- <|> NoMod TabKey                               <$  E.word "<tab>"
  -- <|> NoMod ReturnKey                            <$  E.word "<ret>"
  -- <|> NoMod DeleteKey                            <$  E.word "<del>"
  -- <|> NoMod EscapeKey                            <$  E.word "<esc>"
  --
  -- <|> NoMod UpArrowKey     <$ E.word  "<up>"
  -- <|> NoMod DownArrowKey   <$ E.word  "<down>"
  -- <|> NoMod LeftArrowKey   <$ E.word  "<left>"
  -- <|> NoMod RightArrowKey  <$ E.word  "<right>"
  --
  -- <|> NoMod F1Key <$ E.word  "<f1>"
  -- <|> NoMod F2Key <$ E.word  "<f2>"
  -- <|> NoMod F3Key <$ E.word  "<f3>"
  -- <|> NoMod F4Key <$ E.word  "<f4>"
  -- <|> NoMod F5Key <$ E.word  "<f5>"
  -- <|> NoMod F6Key <$ E.word  "<f6>"
  -- <|> NoMod F7Key <$ E.word  "<f7>"
  -- <|> NoMod F8Key <$ E.word  "<f8>"
  -- <|> NoMod F9Key <$ E.word  "<f9>"
  -- <|> NoMod F10Key <$ E.word  "<f10>"
  -- <|> NoMod F11Key <$ E.word  "<f11>"
  -- <|> NoMod F12Key <$ E.word  "<f12>"
  -- <|> NoMod F13Key <$ E.word  "<f13>"
  -- <|> NoMod F14Key <$ E.word  "<f14>"
  -- <|> NoMod F15Key <$ E.word  "<f15>"
  -- <|> NoMod F16Key <$ E.word  "<f16>"
  -- <|> NoMod F17Key <$ E.word  "<f17>"
  -- <|> NoMod F18Key <$ E.word  "<f18>"
  -- <|> NoMod F19Key <$ E.word  "<f19>"
  -- <|> NoMod F20Key <$ E.word  "<f20>"
