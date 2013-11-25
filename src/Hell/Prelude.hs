{-# LANGUAGE CPP, ScopedTypeVariables #-}
-- | A base set of functions for the shell.

module Hell.Prelude where

import Prelude
import System.Directory
import System.Exit
import System.IO
#ifdef USE_OLD_TIME
#else
import Data.Time.Clock
#endif
import System.Process
import Data.Maybe
import Data.Typeable
import Control.Applicative
import Text.Regex.Posix

-- TODO - Remove "/bin/sh: 1: " from error messages; have a way to give ghc error (prefix?); prevent ctrl+c from leaving.

-- | setCurrentDirectory
cd :: FilePath -> IO ()
cd = setCurrentDirectory

-- | system cmd
run' :: String -> IO ExitCode
run' cmd = system cmd

-- Returns Just with IO to run if there was a cd command, and Nothing if it should be run normally.
runWithExtractedCd :: String -> Maybe (IO String)
runWithExtractedCd cmd = let (_, _, _, groups) = cmd =~ "^ *cd ([^ \"]+)" :: (String, String, String, [String])
                         in if length groups == 1
                            then Just $ cd (head groups) >> return ""
                            else Nothing
-- TODO - Capture anywhere using "^[^\"]*(\"[^\"]\")*[\"]*cd ([^ \"]+)"

-- Run with stdin input
run_ :: String -> IO String
run_ cmd | isJust (runWithExtractedCd cmd) = fromJust $ runWithExtractedCd cmd
run_ cmd = do out <- pipeThroughCmd cmd (Right $ Inherit) CreatePipe
              case out of
                Left s -> return s
                Right (UseHandle h) -> hGetContents h
                Right _ -> return ""

-- Run multiple piped commands (with string input into first)
run :: String -> String -> IO String
run cmd _ | isJust (runWithExtractedCd cmd) = fromJust $ runWithExtractedCd cmd
run cmd input = do out <- pipeThroughCmd cmd (Left $ input) CreatePipe
                   case out of
                    Left s -> return s
                    Right (UseHandle h) -> hGetContents h
                    Right _ -> return ""

pipeThroughCmd :: String -> Either String StdStream -> StdStream -> IO (Either String StdStream)
pipeThroughCmd cmd pipe_in pipe_out =
                   do d <- getCurrentDirectory
                      (stdin_m, stdout_m, _, p) <- createProcess $
                        CreateProcess
                          { cmdspec = ShellCommand cmd
                          , cwd = Just d
                          , env = Nothing
                          , std_out = pipe_out
                          , std_in = case pipe_in of
                                      Right stream -> stream
                                      Left _ -> CreatePipe
                          , std_err = Inherit
                          , close_fds = False
                          , create_group = False
                          }
                      case pipe_in of
                        Right _ -> return ()
                        Left s -> maybe (return ()) (flip hPutStr s) stdin_m
                      _ <- waitForProcess p
                      return $ maybe (Left "") (Right . UseHandle) stdout_m

toString :: (Typeable a, Show a) => a -> String
toString = liftA2 fromMaybe show cast

