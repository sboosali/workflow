module Workflow.Windows.Extra
 ( module Workflow.Windows.Extra
 , module Control.Arrow
 , module Data.Data
 , module GHC.Generics
 , module Data.Function
 , module Data.Foldable
 , module Export_
 ) where

import Control.Arrow ((>>>))
import Data.Data (Data)
import GHC.Generics (Generic)
import Data.Function ((&),on)
import Data.Foldable (traverse_)
import Control.Monad as Export_
import Foreign (Ptr,nullPtr)

nothing :: IO ()
nothing = return ()

{-|

(NOTE truncates large integral types).

-}
toInt :: (Integral a) => a -> Int
toInt = toInteger >>> (id :: Integer -> Integer) >>> fromIntegral

isNull :: Ptr a -> Bool
isNull = (== nullPtr)
