module Lib (execScheme) where

import Data.List

import Parser.Base
import Parser.Scheme
import Parser.Error
import Eval.Scheme
import Eval.Error
import Tokens
import Data.Char (isSpace)


data ExecutionError = Perror ParseError Integer
                    | Eerror EvalError Integer

instance Show ExecutionError where
  show err = case err of
    Perror e line -> "[line " ++ show line ++ "] " ++ show e
    Eerror e line -> "[line " ++ show line ++ "] " ++ show e


-- | Parse and evaluate multiple scheme line
execScheme :: String -> [(String, Scheme)] -> (Either ExecutionError Scheme, [(String, Scheme)])
execScheme str vars = execLines (exprToLines $ trim str) 0 vars


-- | Exec lines reccursively
execLines :: [String] -> Integer -> [(String, Scheme)] -> (Either ExecutionError Scheme, [(String, Scheme)])
execLines [] _ vars = (Right Void, vars)
execLines (x:[]) lcount vars = execSchemeLine x lcount vars
execLines (x:xs) lcount vars = exec x xs
  where
    exec line next = case execSchemeLine line lcount vars of
      (Right _, resVars)  -> execLines next (lcount + 1) resVars
      (Left err, resVars) -> (Left err, resVars)


trim :: String -> String
trim = _trim . _trim
  where _trim = reverse . dropWhile isSpace


-- | Expression to lines
exprToLines :: String -> [String]
exprToLines expr = filter (not . null) $ case consumeLine expr [] 0 0 of
  ([], []) -> []
  ("\n", []) -> []
  (line, []) -> [line]
  (line, rest) -> (line:exprToLines rest)


-- | consume first line and return a tuple with first line and the rest
consumeLine :: String -> String -> Int -> Int -> (String, String)
consumeLine [] line _ _ = (line, [])
consumeLine (c:cs) line l r
  | l == r
    && l /= 0
    && r /= 0 = (line, (c:cs))
  | c == '('  = consumeLine cs (line ++ [c]) (l + 1) r
  | c == ')'  = consumeLine cs (line ++ [c]) l (r + 1)
  | c == ';'  = takeCom cs
  | otherwise = consumeLine cs (line ++ [c]) l r
  where
    takeCom s = consumeLine (fLines s) line l r
    fLines = intercalate "\n" . tail . (filter $ not . null) . lines

-- | Parse and evaluate one scheme line
execSchemeLine :: String -> Integer -> [(String, Scheme)] -> (Either ExecutionError Scheme, [(String, Scheme)])
execSchemeLine str count vars = case applyParser parseScheme str of
  Right expr -> case evalScheme expr vars of
    (Right res, resVars) -> (Right res, resVars)
    (Left err, resVars)  -> (Left $ (Eerror err) count, resVars)
  Left err -> (Left $ (Perror err) count, vars)
