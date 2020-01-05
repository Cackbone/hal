module Main where

import System.IO
import System.Environment


import Tokens
import Exec
import LibsLoader
import Utils
import REPL
import Files


main :: IO [(String, Scheme)]
main = getArgs >>= parseArgs


-- | Parse arguments
parseArgs :: [String] -> IO [(String, Scheme)]
parseArgs args
  | length args == 0 = exitWithErr "You should provide at least one file or use flag -i for interactive mode."
  | (length args /= 0) && last args == "-i" = do
      std <- stdLoader
      libs <- libsLoader $ init args
      interpreter $ std ++ libs
  | (length args /= 0) && head args == "-i" = do
      vars <- stdLoader >>= interpreter
      filesInterpreter (tail args) execExprs vars
  | otherwise = filesInterpreter args execExprs []

