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
    , configPrompt = \username pwd -> let white = "\x1b[37m"
                                          green = "\x1b[32m"
                                          magenta = "\x1b[35m"
                                          yellow = "\x1b[33m"
                                      in return $ foldl1 (++) [green, username, white, ":", magenta, pwd, yellow, " λ ", white]
    , configRun = Nothing
    }
