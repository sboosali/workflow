module Workflow.Windows.Extra
 ( module Workflow.Windows.Extra
 , module Control.Arrow
 , module Data.Data
 ) where
import Workflow.Windows.Types

import Control.Arrow ((>>>))
import Control.Concurrent (threadDelay)
import Data.Data (Data)
import GHC.Generics (Generic)


nothing :: IO ()
nothing = return ()

toDWORD :: (Integral a) => a -> DWORD
toDWORD = fromIntegral

delay :: Int -> IO ()
delay = threadDelay . (*1000)

{-|

(NOTE truncates large integral types).

-}
toInt :: (Integral a) => a -> Int
toInt = toInteger >>> (id :: Integer -> Integer) >>> fromIntegral
