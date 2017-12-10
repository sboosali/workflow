{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}
module Workflow.Pure.Char where
import Workflow.Types
import Workflow.Pure.Extra


{- |

>>> int2keychord -12
[([],MinusKey),([],OneKey),([],TwoKey)]


-}
int2keychord :: Integer -> [KeyChord]
int2keychord = concatMap char2keychord . show

{- |

a (base ten) digit is a number between zero and nine inclusive.

>>> digit2keychord 2
([],TwoKey)

>>> digit2keychord -2
Nothing

>>> digit2keychord 12
Nothing

-}
digit2keychord :: (MonadThrow m, Num a, Eq a, Show a) => a -> m KeyChord

digit2keychord 0  = return $ SimpleKeyChord ZeroKey
digit2keychord 1  = return $ SimpleKeyChord OneKey
digit2keychord 2  = return $ SimpleKeyChord TwoKey
digit2keychord 3  = return $ SimpleKeyChord ThreeKey
digit2keychord 4  = return $ SimpleKeyChord FourKey
digit2keychord 5  = return $ SimpleKeyChord FiveKey
digit2keychord 6  = return $ SimpleKeyChord SixKey
digit2keychord 7  = return $ SimpleKeyChord SevenKey
digit2keychord 8  = return $ SimpleKeyChord EightKey
digit2keychord 9  = return $ SimpleKeyChord NineKey

digit2keychord k = failed $
 "digit2keychord: digits must be between zero and nine: " ++ show k

--note matching on generic numeric literals demands (Num a, Eq a)


{- | the keychord that would insert the character into the application.

>>> char2keychord '@' :: Maybe KeyChord
Just ([ShiftModifier], TwoKey)

some characters cannot be represented as keychordes, like some non-printable characters
(in arbitrary applications, not just the terminal emulator):

>>> char2keychord '\0' :: Maybe KeyChord
Nothing

prop> case char2keychord c of {  Just ([],_) -> True;  Just ([ShiftModifier],_) -> True;  Nothing -> True;  _ -> False  }

-}
char2keychord :: (MonadThrow m) => Char -> m KeyChord -- (KeyChord [Modifier] Key)

char2keychord 'a'  = return $ (,) []              AKey
char2keychord 'A'  = return $ (,) [ShiftModifier] AKey
char2keychord 'b'  = return $ (,) []              BKey
char2keychord 'B'  = return $ (,) [ShiftModifier] BKey
char2keychord 'c'  = return $ (,) []              CKey
char2keychord 'C'  = return $ (,) [ShiftModifier] CKey
char2keychord 'd'  = return $ (,) []              DKey
char2keychord 'D'  = return $ (,) [ShiftModifier] DKey
char2keychord 'e'  = return $ (,) []              EKey
char2keychord 'E'  = return $ (,) [ShiftModifier] EKey
char2keychord 'f'  = return $ (,) []              FKey
char2keychord 'F'  = return $ (,) [ShiftModifier] FKey
char2keychord 'g'  = return $ (,) []              GKey
char2keychord 'G'  = return $ (,) [ShiftModifier] GKey
char2keychord 'h'  = return $ (,) []              HKey
char2keychord 'H'  = return $ (,) [ShiftModifier] HKey
char2keychord 'i'  = return $ (,) []              IKey
char2keychord 'I'  = return $ (,) [ShiftModifier] IKey
char2keychord 'j'  = return $ (,) []              JKey
char2keychord 'J'  = return $ (,) [ShiftModifier] JKey
char2keychord 'k'  = return $ (,) []              KKey
char2keychord 'K'  = return $ (,) [ShiftModifier] KKey
char2keychord 'l'  = return $ (,) []              LKey
char2keychord 'L'  = return $ (,) [ShiftModifier] LKey
char2keychord 'm'  = return $ (,) []              MKey
char2keychord 'M'  = return $ (,) [ShiftModifier] MKey
char2keychord 'n'  = return $ (,) []              NKey
char2keychord 'N'  = return $ (,) [ShiftModifier] NKey
char2keychord 'o'  = return $ (,) []              OKey
char2keychord 'O'  = return $ (,) [ShiftModifier] OKey
char2keychord 'p'  = return $ (,) []              PKey
char2keychord 'P'  = return $ (,) [ShiftModifier] PKey
char2keychord 'q'  = return $ (,) []              QKey
char2keychord 'Q'  = return $ (,) [ShiftModifier] QKey
char2keychord 'r'  = return $ (,) []              RKey
char2keychord 'R'  = return $ (,) [ShiftModifier] RKey
char2keychord 's'  = return $ (,) []              SKey
char2keychord 'S'  = return $ (,) [ShiftModifier] SKey
char2keychord 't'  = return $ (,) []              TKey
char2keychord 'T'  = return $ (,) [ShiftModifier] TKey
char2keychord 'u'  = return $ (,) []              UKey
char2keychord 'U'  = return $ (,) [ShiftModifier] UKey
char2keychord 'v'  = return $ (,) []              VKey
char2keychord 'V'  = return $ (,) [ShiftModifier] VKey
char2keychord 'w'  = return $ (,) []              WKey
char2keychord 'W'  = return $ (,) [ShiftModifier] WKey
char2keychord 'x'  = return $ (,) []              XKey
char2keychord 'X'  = return $ (,) [ShiftModifier] XKey
char2keychord 'y'  = return $ (,) []              YKey
char2keychord 'Y'  = return $ (,) [ShiftModifier] YKey
char2keychord 'z'  = return $ (,) []              ZKey
char2keychord 'Z'  = return $ (,) [ShiftModifier] ZKey

char2keychord '0'  = return $ (,) []              ZeroKey
char2keychord ')'  = return $ (,) [ShiftModifier] ZeroKey
char2keychord '1'  = return $ (,) []              OneKey
char2keychord '!'  = return $ (,) [ShiftModifier] OneKey
char2keychord '2'  = return $ (,) []              TwoKey
char2keychord '@'  = return $ (,) [ShiftModifier] TwoKey
char2keychord '3'  = return $ (,) []              ThreeKey
char2keychord '#'  = return $ (,) [ShiftModifier] ThreeKey
char2keychord '4'  = return $ (,) []              FourKey
char2keychord '$'  = return $ (,) [ShiftModifier] FourKey
char2keychord '5'  = return $ (,) []              FiveKey
char2keychord '%'  = return $ (,) [ShiftModifier] FiveKey
char2keychord '6'  = return $ (,) []              SixKey
char2keychord '^'  = return $ (,) [ShiftModifier] SixKey
char2keychord '7'  = return $ (,) []              SevenKey
char2keychord '&'  = return $ (,) [ShiftModifier] SevenKey
char2keychord '8'  = return $ (,) []              EightKey
char2keychord '*'  = return $ (,) [ShiftModifier] EightKey
char2keychord '9'  = return $ (,) []              NineKey
char2keychord '('  = return $ (,) [ShiftModifier] NineKey

char2keychord '`'  = return $ (,) []              GraveKey
char2keychord '~'  = return $ (,) [ShiftModifier] GraveKey
char2keychord '-'  = return $ (,) []              MinusKey
char2keychord '_'  = return $ (,) [ShiftModifier] MinusKey
char2keychord '='  = return $ (,) []              EqualKey
char2keychord '+'  = return $ (,) [ShiftModifier] EqualKey
char2keychord '['  = return $ (,) []              LeftBracketKey
char2keychord '{'  = return $ (,) [ShiftModifier] LeftBracketKey
char2keychord ']'  = return $ (,) []              RightBracketKey
char2keychord '}'  = return $ (,) [ShiftModifier] RightBracketKey
char2keychord '\\' = return $ (,) []              BackslashKey
char2keychord '|'  = return $ (,) [ShiftModifier] BackslashKey
char2keychord ';'  = return $ (,) []              SemicolonKey
char2keychord ':'  = return $ (,) [ShiftModifier] SemicolonKey
char2keychord '\'' = return $ (,) []              QuoteKey
char2keychord '"'  = return $ (,) [ShiftModifier] QuoteKey
char2keychord ','  = return $ (,) []              CommaKey
char2keychord '<'  = return $ (,) [ShiftModifier] CommaKey
char2keychord '.'  = return $ (,) []              PeriodKey
char2keychord '>'  = return $ (,) [ShiftModifier] PeriodKey
char2keychord '/'  = return $ (,) []              SlashKey
char2keychord '?'  = return $ (,) [ShiftModifier] SlashKey

char2keychord ' '  = return $ (,) []              SpaceKey
char2keychord '\t' = return $ (,) []              TabKey
char2keychord '\n' = return $ (,) []              ReturnKey

char2keychord c = failed $
  "char2keychord: un-pressable Char: " ++ show c


{- | the character that represents the keychord:

>>> keychord2char ([ShiftModifier], TwoKey) :: Maybe Char
Just '@'

some keychordes cannot be represented as characters, like keyboard shortcuts:

>>> keychord2char ([Command], CKey) :: Maybe Char
Nothing

>>> import Data.Char
>>> import Data.Maybe
prop> maybe True isAscii (keychord2char k)
TODO replace true with redo test

-}
keychord2char :: (MonadThrow m) => KeyChord -> m Char

keychord2char (KeyChord []              AKey)            = return 'a'
keychord2char (KeyChord [ShiftModifier] AKey)            = return 'A'
keychord2char (KeyChord []              BKey)            = return 'b'
keychord2char (KeyChord [ShiftModifier] BKey)            = return 'B'
keychord2char (KeyChord []              CKey)            = return 'c'
keychord2char (KeyChord [ShiftModifier] CKey)            = return 'C'
keychord2char (KeyChord []              DKey)            = return 'd'
keychord2char (KeyChord [ShiftModifier] DKey)            = return 'D'
keychord2char (KeyChord []              EKey)            = return 'e'
keychord2char (KeyChord [ShiftModifier] EKey)            = return 'E'
keychord2char (KeyChord []              FKey)            = return 'f'
keychord2char (KeyChord [ShiftModifier] FKey)            = return 'F'
keychord2char (KeyChord []              GKey)            = return 'g'
keychord2char (KeyChord [ShiftModifier] GKey)            = return 'G'
keychord2char (KeyChord []              HKey)            = return 'h'
keychord2char (KeyChord [ShiftModifier] HKey)            = return 'H'
keychord2char (KeyChord []              IKey)            = return 'i'
keychord2char (KeyChord [ShiftModifier] IKey)            = return 'I'
keychord2char (KeyChord []              JKey)            = return 'j'
keychord2char (KeyChord [ShiftModifier] JKey)            = return 'J'
keychord2char (KeyChord []              KKey)            = return 'k'
keychord2char (KeyChord [ShiftModifier] KKey)            = return 'K'
keychord2char (KeyChord []              LKey)            = return 'l'
keychord2char (KeyChord [ShiftModifier] LKey)            = return 'L'
keychord2char (KeyChord []              MKey)            = return 'm'
keychord2char (KeyChord [ShiftModifier] MKey)            = return 'M'
keychord2char (KeyChord []              NKey)            = return 'n'
keychord2char (KeyChord [ShiftModifier] NKey)            = return 'N'
keychord2char (KeyChord []              OKey)            = return 'o'
keychord2char (KeyChord [ShiftModifier] OKey)            = return 'O'
keychord2char (KeyChord []              PKey)            = return 'p'
keychord2char (KeyChord [ShiftModifier] PKey)            = return 'P'
keychord2char (KeyChord []              QKey)            = return 'q'
keychord2char (KeyChord [ShiftModifier] QKey)            = return 'Q'
keychord2char (KeyChord []              RKey)            = return 'r'
keychord2char (KeyChord [ShiftModifier] RKey)            = return 'R'
keychord2char (KeyChord []              SKey)            = return 's'
keychord2char (KeyChord [ShiftModifier] SKey)            = return 'S'
keychord2char (KeyChord []              TKey)            = return 't'
keychord2char (KeyChord [ShiftModifier] TKey)            = return 'T'
keychord2char (KeyChord []              UKey)            = return 'u'
keychord2char (KeyChord [ShiftModifier] UKey)            = return 'U'
keychord2char (KeyChord []              VKey)            = return 'v'
keychord2char (KeyChord [ShiftModifier] VKey)            = return 'V'
keychord2char (KeyChord []              WKey)            = return 'w'
keychord2char (KeyChord [ShiftModifier] WKey)            = return 'W'
keychord2char (KeyChord []              XKey)            = return 'x'
keychord2char (KeyChord [ShiftModifier] XKey)            = return 'X'
keychord2char (KeyChord []              YKey)            = return 'y'
keychord2char (KeyChord [ShiftModifier] YKey)            = return 'Y'
keychord2char (KeyChord []              ZKey)            = return 'z'
keychord2char (KeyChord [ShiftModifier] ZKey)            = return 'Z'

keychord2char (KeyChord []              ZeroKey)         = return '0'
keychord2char (KeyChord [ShiftModifier] ZeroKey)         = return ')'
keychord2char (KeyChord []              OneKey)          = return '1'
keychord2char (KeyChord [ShiftModifier] OneKey)          = return '!'
keychord2char (KeyChord []              TwoKey)          = return '2'
keychord2char (KeyChord [ShiftModifier] TwoKey)          = return '@'
keychord2char (KeyChord []              ThreeKey)        = return '3'
keychord2char (KeyChord [ShiftModifier] ThreeKey)        = return '#'
keychord2char (KeyChord []              FourKey)         = return '4'
keychord2char (KeyChord [ShiftModifier] FourKey)         = return '$'
keychord2char (KeyChord []              FiveKey)         = return '5'
keychord2char (KeyChord [ShiftModifier] FiveKey)         = return '%'
keychord2char (KeyChord []              SixKey)          = return '6'
keychord2char (KeyChord [ShiftModifier] SixKey)          = return '^'
keychord2char (KeyChord []              SevenKey)        = return '7'
keychord2char (KeyChord [ShiftModifier] SevenKey)        = return '&'
keychord2char (KeyChord []              EightKey)        = return '8'
keychord2char (KeyChord [ShiftModifier] EightKey)        = return '*'
keychord2char (KeyChord []              NineKey)         = return '9'
keychord2char (KeyChord [ShiftModifier] NineKey)         = return '('

keychord2char (KeyChord []              GraveKey)        = return '`'
keychord2char (KeyChord [ShiftModifier] GraveKey)        = return '~'
keychord2char (KeyChord []              MinusKey)        = return '-'
keychord2char (KeyChord [ShiftModifier] MinusKey)        = return '_'
keychord2char (KeyChord []              EqualKey)        = return '='
keychord2char (KeyChord [ShiftModifier] EqualKey)        = return '+'
keychord2char (KeyChord []              LeftBracketKey)  = return '['
keychord2char (KeyChord [ShiftModifier] LeftBracketKey)  = return '{'
keychord2char (KeyChord []              RightBracketKey) = return ']'
keychord2char (KeyChord [ShiftModifier] RightBracketKey) = return '}'
keychord2char (KeyChord []              BackslashKey)    = return '\\'
keychord2char (KeyChord [ShiftModifier] BackslashKey)    = return '|'
keychord2char (KeyChord []              SemicolonKey)    = return ';'
keychord2char (KeyChord [ShiftModifier] SemicolonKey)    = return ':'
keychord2char (KeyChord []              QuoteKey)        = return '\''
keychord2char (KeyChord [ShiftModifier] QuoteKey)        = return '"'
keychord2char (KeyChord []              CommaKey)        = return ','
keychord2char (KeyChord [ShiftModifier] CommaKey)        = return '<'
keychord2char (KeyChord []              PeriodKey)       = return '.'
keychord2char (KeyChord [ShiftModifier] PeriodKey)       = return '>'
keychord2char (KeyChord []              SlashKey)        = return '/'
keychord2char (KeyChord [ShiftModifier] SlashKey)        = return '?'

keychord2char (KeyChord []              SpaceKey)        = return ' '
keychord2char (KeyChord []              TabKey)          = return '\t'
keychord2char (KeyChord []              ReturnKey)       = return '\n'

keychord2char k = failed $
 "keychord2char: non-unicode-representable keychord: " ++ show k

-- TODO partial isomorphism between char2keychord and keychord2char?
