{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}

{-|


Shells out to the following executables:

* @xdotool@
* @xwininfo@
* @xclip@
* @xdg-open@

You can statically ensure that these dependencies are installed by building the package with `stack --nix`. You can also dynamically double-check with 'checkExternalDependencies', which simply check that these executable names are on the path.

-}
module Workflow.Backends.X11.Bindings where
import Workflow.Backends.X11.Extra
import Workflow.Core
--import Workflow.Backends.X11.Types
import Workflow.Backends.X11.Foreign

import Workflow.Types
import Text.Earley

import Data.Word
import System.Process
import Data.Maybe
import System.Exit
import Data.Monoid (All(..))
import Data.Foldable (fold)
import Numeric (showHex)

import Prelude.Spiros
import Prelude ()
  
--------------------------------------------------------

{- | 
-}
sendKey :: (MonadIO m) => Modifiers -> Key -> m ()
sendKey ms k = do
--  isModifierKey k
  -- k' <- (isAlphabeticKey&predicate2validator) k
  -- if isAlphabeticKey k then displayKey k
  let ms' = marshallModifier <$> ms
  let k' = marshallKey k
  unsafeRunShell$ xdotool_key (intercalate "+" (ms' ++ [k']))

xdotool_key t = xdotool_ ["key", "--clearmodifiers", t]

marshallKey k = case groupKey k of
 AlphabeticKey c -> [c] 
 NumericKey k -> show k
 FuncKey k -> "F" <> show k
 ModifierKey m -> marshallModifier m
 UnliteralKey s -> s

marshallModifier = \case
 HyperModifier    -> "ctrl" 
 MetaModifier     -> "alt" 
 -- "actual, virtual" modifiers
 ControlModifier  -> "ctrl"
 OptionModifier   -> "alt"
 ShiftModifier    -> "shift"
 FunctionModifier -> "function" --  TODO?
         
-- sendText :: _
-- sendText = sendText_byKey

{- |

Doesn't support Unicode. TODO: Throw, or filter out, or convert?

-}
sendText_byKey :: (MonadIO m) => String -> m ()
sendText_byKey t = unsafeRunShell $ xdotool_ ["type", t]

{-

{- | 
-}
sendKeyTo :: (MonadIO m) => Window -> Modifiers -> Key -> m ()
sendKeyTo w ms k = do
  return undefined
  
{- | 
-}
getCurrentWindow :: (MonadIO m) => m Window
getCurrentWindow = do
  return undefined

sendText_byChar :: _
sendText_byChar = undefined
 
sendText_byKey :: _
sendText_byKey = undefined
  
sendText_byClipboard :: _
sendText_byClipboard = undefined
  
sendKeyChord_flags :: _
sendKeyChord_flags = undefined
  
sendMouseScroll :: _
sendMouseScroll = undefined
  
-}

sendMouseClick' :: (MonadIO m) => [Modifier] -> Natural     -> MouseButton -> m ()
sendMouseClick' _modifiers repetitions button = do -- TODO modifiers
  unsafeRunShell$ xdotool_click (show repetitions) (marshallButton button)

marshallButton :: MouseButton -> String
marshallButton = \case
 LeftButton   -> "1"
 MiddleButton -> "2"
 RightButton  -> "3"

marshallMouseScroll :: MouseScroll -> Maybe String
marshallMouseScroll = \case
  ScrollTowards -> Just ("5") -- down
  ScrollAway    -> Just ("4") -- up
  ScrollRight   -> Nothing
  ScrollLeft    -> Nothing
  
{-|

left = 1, middle = 2, right = 3, wheel up = 4, wheel down = 5

-}
xdotool_click repetitions button = xdotool_ ["click", "--repeat", repetitions, button]

getClipboard' :: (MonadIO m) => m String
getClipboard' = do
  unsafeRunShell $ xclip ["-selection", "clipboard", "-o"]
  
setClipboard' :: (MonadIO m) => String -> m ()
setClipboard' s = do
  unsafeRunShell $ xclipWith ["-selection", "clipboard"] s

{- | 
-}
currentApplication' :: (MonadIO m) => m Application
currentApplication' = unsafeRunShell $ do
  w' <- xdotool ["getactivewindow"]
  w <- toMonadShell $ parseWindow w'
  cs' <- xprop ["-notype", "WM_CLASS", "-id", show w] -- xdotool ["getwindowclass"] -- TODO "xdotool getwindowclass" doesn't exist
  a <- toMonadShell $ parseApplication w cs'
  return a

  where
  
  -- resolveWindow :: (MonadShell m) => WindowID -> m Application
  -- resolveWindow w = do
  --   todo

  -- handle :: (MonadShell m) => String -> m Application
  -- handle = do
  --   let a = resolveWindow w    

  {-

  e.g. parse the following into "emacs":
    
    WM_CLASS = "emacs", "Emacs"

  -}
  parseApplication :: WindowID -> String -> Either ShellError Application
  parseApplication w ts = maybe2either (ShellError 0 message) $ do --TODO hacky
    t <- listToMaybe $ lines ts
    cs <- parseWMClasses t
    let c = cs & listToMaybe & maybe "" id  -- pick first class, empty otherwise
    return c
    where
    message = "[Workflow.X11.currentApplication] " <> show ts <> " didn't parse as a WM_CLASS" -- Show the string to escape new lines 


  -- {-

  -- e.g. parse the following into "emacs":

  --    0x1600014 "emacs@chez-sboo": ("emacs" "Emacs")  958x948+0+0  +0+0

  -- -}
  -- parseApplication :: WindowID -> String -> Either ShellError Application
  -- parseApplication w0 ss = maybe2either (ShellError 0 message) $ do --TODO hacky
  --   -- s <- listToMaybe $ lines ss
  --   let w1 = asHex w0
  --   let cs = [] -- If title has colons or escape quotes?
  --   let c = maybe "" id . listToMaybe $ cs -- pick first class, empty otherwise
  --   return c
  --   where
  --   y = "(has no name)"
  --   asHex x = "0x" <> showHex x ""
  --   message = "[Workflow.X11.currentApplication] " <> show ss <> " didn't parse as an Application" -- Show the string to escape new lines 

  parseWindow :: String -> Either ShellError WindowID 
  parseWindow ss = maybe2either (ShellError 0 message) $ do
    s <- listToMaybe $ lines ss
    i <- readMay s
    return i
    where
    message = "[Workflow.X11.currentApplication] " <> show ss <> " didn't parse as a WindowID" -- Show the string to escape new lines 

{-
$ sleep 1; xdotool getactivewindow
23068692
$ printf 0x%x 23068692               
0x42002bc
$ xwininfo -root -tree | grep 0x1600014
     0x1600014 "emacs@chez-sboo": ("emacs" "Emacs")  958x948+0+0  +0+0

-}

  -- readProcessWithExitCode "xdotool" ["getactivewindow"] "" <&> handle
  -- where
  -- handle = \case
  --    (ExitSuccess, (parse -> Just i), "") -> Right i
  --    _ -> Left ""
  -- parse :: String -> Maybe Integer -- x11 process identifiers are word64's
  -- parse ss = do
  --   s <- listToMaybe $ lines ss
  --   i <- readMay s
  --   return i

{-|
    
>>> parseWMClasses "WM_CLASS = \"emacs\", \"Emacs\""
Just ["emacs", "Emacs"]

>>> (traverse_.traverse) putStrLn $ parseWMClasses "WM_CLASS = \"\\\"\""
"

-}
parseWMClasses :: String -> Maybe [String]
parseWMClasses = go
    where
    go = fullParses (parser xAtomGrammar) > fst > argmax length -- the longest ("least-greedy") parse; Earley doesn't return parses in any order (in particular, not in a top-down left-right order)
    -- parse = listToMaybe . fst . fullParses (parser xAtomGrammar) -- the first valid parse
    argmax f = sortOn f > reverse > listToMaybe

xAtomGrammar :: Grammar r (Prod r String Char [String])
xAtomGrammar = do
      char :: Prod r String Char Char <- rule $ empty -- TODO correct?
        <|> '\\' <$ list "\\\\"
        <|> '"' <$ list "\\\""
        <|> satisfy (\c -> not (c == '\\')) -- (:[]) <$> 
      inner <- rule $ many char --  concat <$> 
      quoted <- rule $ token '\"' *> (inner <* token '\"')
      classes <- rule $ interweaveA (list ", ") quoted
      rule $ list "WM_CLASS = " *> classes
    where
    interweaveA :: Alternative f => f x -> f a -> f [a]
    interweaveA x a = (:) <$> a <*> many (x *> a)  -- intercalate f ( g) & asum

-- Assumes (often incorrectly) that the application's executable and its class are the same
openApplication' :: (MonadIO m) => Application -> m ()
openApplication' _t = do
  todo 
-- link executable with class?

-- via class name
focusApplication' :: (MonadIO m) => Application -> m ()
focusApplication' _t = do
  todo

-- via executable name
launchApplication' :: (MonadIO m) => Application -> m ()
launchApplication' t = do  -- TODO nix pure hides other executables
  unsafeRunShell $ execute_ t [] -- TODO on Linux, an application is launched via the executable name, but accessed via a class name.
-- TODO is it free like with &d

-- | Skipping anonymous windows and duplicates
getOpenApplications ::  (MonadIO m) => m [ApplicationName]
getOpenApplications = getOpenApplications' <&> (mapMaybe listToMaybe > nub)

{-

$ prop -notype "WM_CLASS" -id 641
WM_CLASS:  not found.
$ echo $?
0

"for WindowId in $(xdotool search '.*'); do xprop -notype 'WM_CLASS' -id $WindowId; done 2> /dev/null"

-}
getOpenApplications' ::  (MonadIO m) => m [[ApplicationName]]
getOpenApplications' = do
  o <- sh "for WindowId in $(xdotool search '.*'); do xprop -notype 'WM_CLASS' -id $WindowId; done 2> /dev/null"
  return $ go o
  where
  go = lines > fmap (parseWMClasses > maybe [] id)

openURL' :: (MonadIO m) => URL -> m ()
openURL' t = do
  unsafeRunShell $ xdgopen_ [t] -- TODO nix pure hides browser executables

-------------------------------------------

  -- x11 process identifiers are word64's
type WindowID = Word64

executableDependencies :: [FilePath]
executableDependencies =["xdotool","xprop","xclip","xdg_open"] 

checkExternalDependencies :: (MonadIO m) => m Bool
checkExternalDependencies = do 

  bs <- traverse check executableDependencies
  let b =  bs & fmap All & fold & getAll
  return b

  where
  check exe = io (system exe) >>= \case
    ExitFailure 127 -> return False  -- pattern COMMAND_NOT_FOUND = 127
    _ -> return True
   
  -- xdotool []
  -- xclip []
  -- xdg_open []

  -- xdotool "--version"
  -- xclip "-h"
  -- xdg_open "--version"

 
-- readProcessWithExitCode  "seq" ["1", "10"] ""
-- ExitSuccess
predicate2validator :: (a -> Bool) -> (a -> Maybe a) -- TODO mv
predicate2validator p a = if p a then Just a else Nothing

---------------------------------

data KeyGroup
 = AlphabeticKey Char -- ^ a-z
 | NumericKey Natural -- ^ 0-9
 | FuncKey Natural -- ^ 1-20
 | ModifierKey Modifier -- ^ 
 | UnliteralKey String -- ^ e.g. Backspace

{-|

(The symbol names were discovered via @xev -event keyboard@).

-}
groupKey :: Key -> KeyGroup
groupKey = \case

 MetaKey -> ModifierKey MetaModifier
 HyperKey -> ModifierKey HyperModifier
 ControlKey -> ModifierKey ControlModifier
 ShiftKey -> ModifierKey ShiftModifier
 OptionKey -> ModifierKey OptionModifier
 FunctionKey -> ModifierKey FunctionModifier

 GraveKey -> UnliteralKey "grave"
 MinusKey -> UnliteralKey "minus"
 EqualKey -> UnliteralKey "equal"
 DeleteKey -> UnliteralKey "Backspace"
 ForwardDeleteKey -> UnliteralKey "Delete"
 LeftBracketKey -> UnliteralKey "bracketleft"
 RightBracketKey -> UnliteralKey "bracketright"
 BackslashKey -> UnliteralKey "backslash"
 SemicolonKey -> UnliteralKey "semicolon"
 QuoteKey -> UnliteralKey "apostrophe"
 CommaKey -> UnliteralKey "comma"
 PeriodKey -> UnliteralKey "period"
 SlashKey -> UnliteralKey "slash"
 TabKey -> UnliteralKey "Tab"
 SpaceKey -> UnliteralKey "Space"
 ReturnKey -> UnliteralKey "Return"
 LeftArrowKey -> UnliteralKey "Left"
 RightArrowKey -> UnliteralKey "Right"
 DownArrowKey -> UnliteralKey "Down"
 UpArrowKey -> UnliteralKey "Up"
 EscapeKey -> UnliteralKey "Escape"
 CapsLockKey -> UnliteralKey "CapsLock" -- TODO?

 AKey -> AlphabeticKey 'a'
 BKey -> AlphabeticKey 'b'
 CKey -> AlphabeticKey 'c'
 DKey -> AlphabeticKey 'd'
 EKey -> AlphabeticKey 'e'
 FKey -> AlphabeticKey 'f'
 GKey -> AlphabeticKey 'g'
 HKey -> AlphabeticKey 'h'
 IKey -> AlphabeticKey 'i'
 JKey -> AlphabeticKey 'j'
 KKey -> AlphabeticKey 'k'
 LKey -> AlphabeticKey 'l'
 MKey -> AlphabeticKey 'm'
 NKey -> AlphabeticKey 'n'
 OKey -> AlphabeticKey 'o'
 PKey -> AlphabeticKey 'p'
 QKey -> AlphabeticKey 'q'
 RKey -> AlphabeticKey 'r'
 SKey -> AlphabeticKey 's'
 TKey -> AlphabeticKey 't'
 UKey -> AlphabeticKey 'u'
 VKey -> AlphabeticKey 'v'
 WKey -> AlphabeticKey 'w'
 XKey -> AlphabeticKey 'x'
 YKey -> AlphabeticKey 'y'
 ZKey -> AlphabeticKey 'z'

 ZeroKey -> NumericKey 0
 OneKey -> NumericKey 1
 TwoKey -> NumericKey 2
 ThreeKey -> NumericKey 3
 FourKey -> NumericKey 4
 FiveKey -> NumericKey 5
 SixKey -> NumericKey 6
 SevenKey -> NumericKey 7
 EightKey -> NumericKey 8
 NineKey -> NumericKey 9

 F1Key -> FuncKey 1
 F2Key -> FuncKey 2
 F3Key -> FuncKey 3
 F4Key -> FuncKey 4
 F5Key -> FuncKey 5
 F6Key -> FuncKey 6
 F7Key -> FuncKey 7
 F8Key -> FuncKey 8
 F9Key -> FuncKey 9
 F10Key -> FuncKey 10
 F11Key -> FuncKey 11
 F12Key -> FuncKey 12
 F13Key -> FuncKey 13
 F14Key -> FuncKey 14
 F15Key -> FuncKey 15
 F16Key -> FuncKey 16
 F17Key -> FuncKey 17
 F18Key -> FuncKey 18
 F19Key -> FuncKey 19
 F20Key -> FuncKey 20
 
