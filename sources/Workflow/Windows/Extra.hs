module Workflow.Windows.Extra
 ( module Workflow.Windows.Extra
 , module Control.Arrow
 , module Data.Data
 , module GHC.Generics
 , module Data.Function
 , module Data.Foldable
 ) where
import Workflow.Windows.Types

import Control.Arrow ((>>>))
import Control.Concurrent (threadDelay)
import Data.Data (Data)
import GHC.Generics (Generic)
import Data.Function ((&),on)
import Data.Foldable (traverse_)
import Control.Monad.IO.Class

nothing :: IO ()
nothing = return ()

toDWORD :: (Integral a) => a -> DWORD
toDWORD = fromIntegral

delayMilliseconds :: (MonadIO m) => Int -> m ()
delayMilliseconds = liftIO . threadDelay . (*1000)

{-|

(NOTE truncates large integral types).

-}
toInt :: (Integral a) => a -> Int
toInt = toInteger >>> (id :: Integer -> Integer) >>> fromIntegral
