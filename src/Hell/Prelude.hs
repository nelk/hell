{-# LANGUAGE CPP, ScopedTypeVariables #-}
-- | A base set of functions for the shell.

module Hell.Prelude where

import Prelude
import Control.Monad
import Data.List
import System.Directory
import System.Exit
import System.IO
#ifdef USE_OLD_TIME
import System.Time
#else
import Data.Time.Clock
#endif
import System.Process
import Data.Maybe
import Data.Typeable
import Control.Applicative
import Data.String.Utils (split)

-- | setCurrentDirectory
cd :: FilePath -> IO ()
cd = setCurrentDirectory

-- | getCurrentDirectory
pwd :: IO FilePath
pwd = getCurrentDirectory

-- | getHomeDirectory
home :: IO FilePath
home = getHomeDirectory

-- | getTemporaryDirectory
tmp :: IO FilePath
tmp = getTemporaryDirectory

-- | removeFile
rm :: FilePath -> IO ()
rm = removeFile

-- | renameFile
mv :: FilePath -> FilePath -> IO ()
mv = renameFile

-- | renameDirectory
mvdir :: FilePath -> FilePath -> IO ()
mvdir = renameDirectory

-- | copyFile
cp :: FilePath -> FilePath -> IO ()
cp = copyFile

-- | findExecutable
whereis :: String -> IO (Maybe FilePath)
whereis = findExecutable

-- | getPermissions
perms :: FilePath -> IO Permissions
perms = getPermissions

-- | getModificationTime
#ifdef USE_OLD_TIME
modified :: FilePath -> IO ClockTime
#else
modified :: FilePath -> IO UTCTime
#endif
modified = getModificationTime

-- | removeDirectory
rmdir :: FilePath -> IO ()
rmdir = removeDirectory

-- | removeDirectoryRecursive
rmdirR :: FilePath -> IO ()
rmdirR = removeDirectoryRecursive

-- | createDirectory
mkdir :: FilePath -> IO ()
mkdir = createDirectory

-- | createDirectoryIfMissing
mkdirF :: Bool -> FilePath -> IO ()
mkdirF = createDirectoryIfMissing

-- | dir' >=> mapM_ putStrLn
dir :: FilePath -> IO ()
dir = dir' >=> mapM_ putStrLn

-- | fmap (sort . filter (not . isPrefixOf \".\")) (getDirectoryContents d)
dir' :: FilePath -> IO [[Char]]
dir' d = fmap (sort . filter (not . isPrefixOf ".")) (getDirectoryContents d)

-- | ls' >>= mapM_ putStrLn
ls :: IO ()
ls = ls' >>= mapM_ putStrLn

-- | dir' \".\"
ls' :: IO [[Char]]
ls' = dir' "."

-- | fmap sort (getDirectoryContents \".\")
lsa :: IO [FilePath]
lsa = fmap sort (getDirectoryContents ".")

-- | readFile
cat :: FilePath -> IO String
cat = readFile

-- | writeFile
write :: FilePath -> String -> IO ()
write = writeFile

-- | appendFile
append :: FilePath -> String -> IO ()
append = appendFile

-- | hFileSize
size :: FilePath -> IO Integer
size fp = do
  h <- openFile fp ReadMode
  s <- hFileSize h
  hClose h
  return s

-- | putStrLn
echo :: String -> IO ()
echo = putStrLn

-- | system cmd
run_ecode :: String -> IO ExitCode
run_ecode cmd = system cmd

-- Run without input
run_ :: String -> IO String
run_ cmd = run cmd ""

-- Run multiple piped commands (with string input into first)
run :: String -> String -> IO String
run combined_cmds input = do out <- foldl (>>=) (head cmds $ Left input) (tail cmds)
                             case out of
                              Left s -> return s
                              Right h -> hGetContents h
  where cmds = map pipe $ split "|" combined_cmds
        pipe :: String -> Either String Handle -> IO (Either String Handle)
        pipe cmd pipe_in = do d <- pwd
                              (stdin_m, stdout_m, _, _) <- createProcess $
                                CreateProcess
                                  { cmdspec = ShellCommand cmd
                                  , cwd = Just d
                                  , env = Nothing
                                  , std_out = CreatePipe
                                  , std_in = case pipe_in of
                                              Right h -> UseHandle h
                                              Left _ -> CreatePipe
                                  , std_err = Inherit
                                  , close_fds = False
                                  , create_group = False
                                  }
                              case pipe_in of
                                Right _ -> return ()
                                Left s -> maybe (return ()) (flip hPutStr input) stdin_m
                              --_ <- waitForProcess p -- Not really necessary.
                              -- TODO handle stderr
                              return $ maybe (Left "") Right stdout_m

toString :: (Typeable a, Show a) => a -> String
toString = liftA2 fromMaybe show cast

