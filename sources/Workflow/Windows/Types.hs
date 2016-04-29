{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Workflow.Windows.Types where

import Foreign.C.Types
import Foreign.C.String
import Data.Word
import GHC.Exts

type LPCWSTR = CWString

type WCHAR_T = CWchar

type UINT = Word32

type WORD = Word

{-|

takes an enum to a virtual key code.

-}
data Keyboard key = Keyboard
 { fromKey      :: key      -> WORD
 -- , fromModifier :: modifier -> WORD
 }
-- deriving (Contravariant)

newtype Application = Application String
 deriving (Show, IsString)

newtype URL = URL String
 deriving (Show, IsString)

data VK
 = VK_CONTROL
 
 | VK_0
 | VK_1
 | VK_2
 | VK_3
 | VK_4
 | VK_5
 | VK_6
 | VK_7
 | VK_8
 | VK_9

 | VK_A
 | VK_B
 | VK_C
 | VK_D
 | VK_E
 | VK_F
 | VK_G
 | VK_H
 | VK_I
 | VK_J
 | VK_K
 | VK_L
 | VK_M
 | VK_N
 | VK_O
 | VK_P
 | VK_Q
 | VK_R
 | VK_S
 | VK_T
 | VK_U
 | VK_V
 | VK_W
 | VK_X
 | VK_Y
 | VK_Z

 deriving (Show,Enum)
