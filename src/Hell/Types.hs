module Hell.Types where

import Data.Default
import GhcMonad

-- | Shell config.
data Config = Config
  { configImports :: ![String] -- ^ Starting imports.
  , configWelcome :: String -- ^ A welcome string.
  , configPrompt  :: String -> FilePath -> Ghc String -- ^ An action to generate the prompt.
  , configRun     :: Maybe String
    -- ^ Generate a string to run statements in, for custom shell
    -- monads. Takes a username, pwd and returns something like
    -- e.g. \"runMyShellMonad\".
  }

instance Default Config where
  def = Config
    { configImports =
        map ("import "++)
            ["Prelude"
             ,"GHC.Types"
             ,"System.IO"
             ,"Data.List"
             ,"Control.Monad"
             ,"Control.Monad.Fix"
             ,"System.Directory"
             ,"System.Process"
             ,"System.Environment"
             ,"Hell.Prelude"
             ,"Data.Maybe"
             ,"Data.Typeable"
             ,"Control.Applicative"
             ]
    , configWelcome = "Welcome to Hell!"
    , configPrompt = \username pwd -> return (username ++ ":" ++ pwd ++ "$ ")
    , configRun = Nothing
    }
