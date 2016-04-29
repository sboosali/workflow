{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module Workflow.Bindings where

import Foreign.C
--import Foreign.C.String
import Data.Word
import Data.Char
import Data.Foldable
import Control.Concurrent

#include "calling_convention.h"


type LPCWSTR = CWString

type WCHAR_T = CWchar

type UINT = Word32

-- foreign import CALLING_CONVENTION unsafe "Workflow.h "


getClipboard :: IO String
getClipboard = c_GetClipboard >>= peekCWString

foreign import CALLING_CONVENTION unsafe "Workflow.h GetClipboard"
 c_GetClipboard :: IO CWString


setClipboard :: String -> IO ()
setClipboard s = withCWString s c_SetClipboard

foreign import CALLING_CONVENTION unsafe "Workflow.h SetClipboard"
 c_SetClipboard :: CWString -> IO ()


{-| inserts char-by-char, no delay.

-}
insertText :: String -> IO ()
insertText = traverse_ insertChar -- TODO delay?

insertTextWith :: Int -> String -> IO ()
insertTextWith i = traverse_ (\c -> insertChar c >> threadDelay i)


insertChar :: Char -> IO ()
insertChar c = do
 _ <- c_InsertUnicodeChar (CWchar (fromIntegral (ord c))) -- cast doesn't overflow, there are ~1,000,000 chars.
 return ()

foreign import CALLING_CONVENTION unsafe "Workflow.h InsertUnicodeChar"
 c_InsertUnicodeChar :: WCHAR_T -> IO UINT


{-|
TODO windows "apps"
launchApplication :: String -> IO ()
launchApplication s = withCWString s c_LaunchApplication
-}

openApplication :: String -> IO ()
openApplication s = withCWString s c_OpenApplication

foreign import CALLING_CONVENTION unsafe "Workflow.h OpenApplication"
 c_OpenApplication :: CWString -> IO ()


openUrl :: String -> IO ()
openUrl s = withCWString s c_OpenUrl

foreign import CALLING_CONVENTION unsafe "Workflow.h OpenUrl"
 c_OpenUrl :: CWString -> IO ()
