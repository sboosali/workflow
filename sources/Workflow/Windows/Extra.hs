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
import Control.Concurrent (threadDelay)
import Data.Data (Data)
import GHC.Generics (Generic)
import Data.Function ((&),on)
import Data.Foldable (traverse_)
import Control.Monad.IO.Class
import Control.Monad as Export_
import Foreign (Ptr,nullPtr)

nothing :: IO ()
nothing = return ()

delayMilliseconds :: (MonadIO m) => Int -> m ()
delayMilliseconds = liftIO . threadDelay . (*1000)

{-|

(NOTE truncates large integral types).

-}
toInt :: (Integral a) => a -> Int
toInt = toInteger >>> (id :: Integer -> Integer) >>> fromIntegral

todo :: a --TODO call stack
todo = error "TODO"

isNull :: Ptr a -> Bool
isNull = (== nullPtr)
