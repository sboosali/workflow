module Workflow.Windows.Extra
 ( module Workflow.Windows.Extra
 , module Control.Arrow
 ) where
import Workflow.Windows.Types

import Control.Arrow ((>>>))
import Control.Concurrent (threadDelay)


nothing :: IO ()
nothing = return ()

toDWORD :: (Integral a) => a -> DWORD
toDWORD = fromIntegral

delay :: Int -> IO ()
delay = threadDelay . (*1000)

{-|

truncates large integral types.

-}
toInt :: (Integral a) => a -> Int
toInt = toInteger >>> (id :: Integer -> Integer) >>> fromIntegral
