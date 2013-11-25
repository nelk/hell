{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

-- | The Hell shell.

module Hell
  (module Hell.Types
  ,module Data.Default
  ,startHell)
  where

import Prelude hiding (catch)
import Hell.Types
import Hell.Prelude (run')

import Control.Exception
import Control.Monad
import Data.Default
import Data.Dynamic
import Data.List
import Data.Maybe
import System.Console.Haskeline hiding (catch)
import System.Console.Haskeline.IO
import System.Directory
import System.Posix.User
import Data.String.Utils

import GHC
import GHC.Paths
import GhcMonad
import DynFlags

-- | Go to hell.
startHell :: Config -> IO ()
startHell Config{..} =
  runGhc
    (Just libdir)
    ( do dflags <- getSessionDynFlags
         void (setSessionDynFlags
                 (setFlags [Opt_ImplicitPrelude, Opt_OverloadedStrings]
                           dflags))
         setImports configImports
         hd <- newHaskelineSession
         home <- io getHomeDirectory
         username <- io getEffectiveUserName
         unless (null configWelcome)
                (io (queryInput hd (outputStrLn configWelcome)))
         hellLoop username home hd
    )
  where hellLoop username home hd =
          do pwd <- io getCurrentDirectory
             prompt <- configPrompt username (stripHome home pwd)
             mline <- io $ catch (queryInput hd (getInputLine prompt)) ioExceptionHandler
             case mline of
               Nothing -> return ()
               Just "" -> hellLoop username home hd -- Suppress output if they just pressed enter.
               Just "\n" -> do io $ cancelInput hd
                               hd' <- newHaskelineSession
                               hellLoop username home hd'
               Just line | line == "exit" || line == "\x4" -> return () -- Exit conditions.
               Just line ->
                 do result <- runStatement (fromMaybe "" configRun) line
                    unless (null result)
                           (io (queryInput hd (outputStr $ fixNewlines result)))
                    hellLoop username home hd

newHaskelineSession :: Ghc InputState
newHaskelineSession = io (initializeInput defaultSettings { historyFile = Just "~/.hell_history"})

ioExceptionHandler :: AsyncException -> IO (Maybe String)
ioExceptionHandler UserInterrupt = return $ Just "\n"
ioExceptionHandler e = throw e

fixNewlines :: String -> String
fixNewlines s = if endswith "\n" s' then s' else s' ++ "\n"
  where s' = replace "\\n" "\n" s

-- | Strip and replace /home/chris/blah with ~/blah.
stripHome :: FilePath -> FilePath -> FilePath
stripHome home path
  | isPrefixOf home path = "~/" ++ dropWhile (=='/') (drop (length home) path)
  | otherwise            = path

haskellPrefix :: String
haskellPrefix = ">"

stripHaskellPrefix :: String -> String
stripHaskellPrefix s | startswith haskellPrefix s = drop (length haskellPrefix) s
                     | otherwise = s

-- | Import the given modules.
setImports :: [String] -> Ghc ()
setImports =
  mapM (fmap IIDecl . parseImportDecl) >=> setContext

toStringCode :: String
toStringCode = "(liftA2 fromMaybe show cast)"

-- | Run the given statement.
runStatement :: String -> String -> Ghc String
runStatement run stmt' = do
  result <- gcatch (fmap Right (dynCompileExpr stmt))
                   (\(e::SomeException) -> return (Left e))
  case result of
    Left{} -> runExpression stmt'
    Right compiled ->
      gcatch (fmap ignoreUnit (io (fromDyn compiled (return "Bad compile."))))
             (\(e::SomeException) -> return (show e))

  where stmt = "(" ++ run ++ "(" ++ (stripHaskellPrefix stmt') ++ ")) >>= return . " ++ toStringCode ++ " :: IO String"
        ignoreUnit "()" = ""
        ignoreUnit x = x

-- | Compile the given expression and evaluate it.
runExpression :: String -> Ghc String
runExpression stmt' = do
  result <- gcatch (fmap Right (dynCompileExpr stmt))
                   (\(e::SomeException) -> return (Left e))
  case result of
    Left err -> if startswith haskellPrefix stmt'
                then return $ show err
                else runInShell stmt'
    Right compiled ->
      gcatch (io (fromDyn compiled (return "Bad compile.")))
             (\(e::SomeException) -> return (show e))

  where stmt = "return (" ++ toStringCode ++ " (" ++ (stripHaskellPrefix stmt') ++ ")) :: IO String"

runInShell :: String -> Ghc String
runInShell stmt = io $ run' stmt >> return ""

-- | Short-hand utility.
io :: IO a -> Ghc a
io = liftIO

-- | Set the given flags.
setFlags :: [ExtensionFlag] -> DynFlags -> DynFlags
setFlags xs dflags = foldl xopt_set dflags xs

