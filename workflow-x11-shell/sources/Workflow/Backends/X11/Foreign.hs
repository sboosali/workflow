{-# LANGUAGE RankNTypes, ConstraintKinds, NoImplicitPrelude, DeriveAnyClass #-}

{-|


e.g.

@
-- get the current clipboard contents
runShell $ xclip ["-selection", "clipboard", "-o"]

-- set the clipboard contents
runShell $ xclipWith ["-selection", "clipboard"] "abc"
@

-}

module Workflow.Backends.X11.Foreign where
import Workflow.Backends.X11.Extra

--import Control.Monad.Except

--import Control.Exception (Exception,throwIO)
import System.Process
import System.Exit
import System.IO
--import Data.Word
-- import Data.Maybe
-- import Control.Exception (evaluate)
import Prelude (error)
 
type MonadShell m = (MonadIO m, MonadError ShellError m)
--type STDERR = String -- TODO (String,Word16)

data ShellError = ShellError
  { shExitCode :: Word16 -- ExitCode
  , shStderr   :: String
  } deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable)
  -- deriving (Show,Read,Eq,Ord,Generic,NFData)
deriving instance Exception ShellError

-- | 'runShell = runExceptT'
runShell :: (MonadIO io) => (forall m. (MonadShell m) => m a) -> io (Either ShellError a)
runShell = runExceptT

-- | 'throwIO's 
unsafeRunShell :: (MonadIO io) => (forall m. (MonadShell m) => m a) -> io a
unsafeRunShell m = runShell m >>= either (throwIO > io) return 

toMonadShell :: (MonadShell m) => Either ShellError a -> m a
toMonadShell = either throwError return

--------------

xdotool :: (MonadShell m) => [String] -> m String
xdotool = execute "xdotool"

-- | https://linux.die.net/man/1/xprop
xprop :: (MonadShell m) => [String] -> m String
xprop = execute "xprop"

xdgopen :: (MonadShell m) => [String] -> m String
xdgopen = execute "xdg-open"

xclip :: (MonadShell m) => [String] -> m String
xclip = execute "xclip"

xdotool_ :: (MonadShell m) => [String] -> m ()
xdotool_ = execute_ "xdotool"

xprop_ :: (MonadShell m) => [String] -> m ()
xprop_ = execute_ "xprop"

xdgopen_ :: (MonadShell m) => [String] -> m ()
xdgopen_ = execute_ "xdg-open"

xdgopen' :: (MonadShell m) => [String] -> m ()
xdgopen' = execute' "xdg-open"

xclip_ :: (MonadShell m) => [String] -> m ()
xclip_ = execute_ "xclip"

-- | with the given stdin
xclipWith :: (MonadShell m) => [String] -> String -> m ()
xclipWith arguments theStdin = do
 e <- io$ withCreateProcess command go
 fromExitCode () "" e
 where
 executable = "xclip"
 command = (proc executable arguments) {std_in = CreatePipe}--, std_out = CreatePipe } -- Inherit v CreatePipe
 go (Just hStdin) _ _ p = do -- TODO partial
   () <- hPutStr hStdin theStdin
   hClose hStdin
   waitForProcess p -- TODO
   --return () -- hGetContents hS
 go _ _ _ _ = error "Workflow.Backends.X11.Foreign.xclip"

---------





------------

--raw :: (MonadShell m) => String -> m String
sh_ :: (MonadIO m) => String -> m ()
sh_ command = void$ sh command

--raw :: (MonadShell m) => String -> m String
sh :: (MonadIO m) => String -> m String
sh command = io $ readCreateProcess (shell command) ""

--Asynchronous I.e. nonblocking
sh' :: (MonadIO m) => String -> m ()
sh' = io . void . spawnCommand

execute_ :: (MonadShell m) => String -> [String] -> m ()
execute_ executable arguments = void $ execute executable arguments

execute :: (MonadShell m) => String -> [String] -> m String
execute executable arguments = do
--  outputs@(exitcode, stdout, stderr) <- readProcessWithExitCode executable arguments ""
  result <- io (readProcessWithExitCode executable arguments "") >>= \case
    
   (ExitSuccess, sout, "") -> return sout
   (e,_,serr) -> throwError (ShellError (exitcode2byte e) serr) 
   -- (ExitFailure _, _, stderr) -> throwError (i, stderr)
   
  return result

execute' :: (MonadShell m) => String -> [String] -> m ()
execute' executable arguments = io $ void $ spawnProcess executable arguments

exitcode2byte :: ExitCode -> Word16
exitcode2byte = \case
  ExitSuccess -> 0
  ExitFailure i -> fromIntegral i

-- | constructs a simple 'MonadShell' action
fromExitCode :: (MonadShell m) => a -> String -> ExitCode -> m a
fromExitCode a e = \case
  ExitSuccess -> return a
  ExitFailure i -> throwError (ShellError (fromIntegral i) e)

