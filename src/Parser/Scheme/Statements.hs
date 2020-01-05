module Parser.Scheme.Statements where

import Control.Applicative

import Tokens
import Parser.Base
import Parser.Scheme.Lists
import Parser.Scheme.Utils


-- | Parse scheme statements
parseStatements :: Parser Scheme
parseStatements = parseCond
                  <|> parseIf
                  <|> parseOr
                  <|> parseAnd


-- | Parse arg in cond
_parseCondArg :: Parser Scheme
_parseCondArg = do
  _ <- parseSpaces
  list <- parseList
  _ <- parseSpaces
  return list


-- | Parse arrow clause in cond
_parseArrowClause :: Parser Scheme
_parseArrowClause = do
  _    <- parseSpaces
          >> parseString "("
          >> parseSpaces
  _arg <- parseArg
  _    <- parseSpaces
          >> parseString "=>"
          >> parseSpaces
  fn   <- parseArg
  _    <- parseSpaces
          >> parseString ")"
          >> parseSpaces
  case fn of
    AbstractFn name names fnArgs isVar -> return $ List
      $ ConsCellData _arg
      $ Apply (Procedure $ AnonFn $ AbstractFn name names fnArgs isVar) [_arg]
    p -> return $ List $ ConsCellData _arg $ Apply p [_arg]


-- | Parse a cond statement
parseCond :: Parser Scheme
parseCond = do
  _ <- parseSpaces
  _ <- parseString "("
       >> parseSpaces
       >> parseString "cond"
       >> parseSpaces
  condArgs <- some $ _parseArrowClause <|> _parseCondArg
  _ <- parseSpaces >> parseString ")"
  case last condArgs of
    List (ConsCellData (Ident x) s) -> if x == "else"
      then return $ Cond $ (init condArgs) ++ [List $ ConsCellData (Bool True) s]
      else return $ Cond condArgs
    _ -> return $ Cond condArgs


-- | Parse if statement
parseIf :: Parser Scheme
parseIf = do
  _ <- parseString "("
    >> parseSpaces
    >> parseString "if"
    >> parseSpaces
  cond <- parseArg
  _then <- parseArg
  _else <- parseArg
  _ <- parseSpaces >> parseString ")"
  return $ If cond _then _else


-- | Parse and statement
parseAnd :: Parser Scheme
parseAnd = do
  _     <- parseString "("
           >> parseSpaces
           >> parseString "and"
           >> parseSpaces
  _args <- many parseArg
  _ <- parseSpaces >> parseString ")"
  return $ And _args


-- | Parse or statement
parseOr :: Parser Scheme
parseOr = do
  _ <- parseString "("
       >> parseSpaces
       >> parseString "or"
       >> parseSpaces
  _args <- many parseArg
  _ <- parseSpaces >> parseString ")"
  return $ Or _args
