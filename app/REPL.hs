module REPL where

import System.IO
import System.Exit

import Tokens
import Exec
import Utils

-- | Interpreter in interactive mode
interpreter :: [(String, Scheme)] -> IO [(String, Scheme)]
interpreter vars = do
  _    <- putStr "> "
  _    <- hFlush stdout
  eof <- isEOF
  if eof
    then return vars
    else do
    expr <- getLine
    resVars <- execExpr expr vars
    interpreter resVars
