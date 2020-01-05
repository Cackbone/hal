module Utils where

import System.IO
import System.Exit

import Tokens

-- | Print error message on stderr and exit 84
exitWithErr :: String -> IO [(String, Scheme)]
exitWithErr err = hPutStrLn stderr err >> exitWith (ExitFailure 84) >> return []
