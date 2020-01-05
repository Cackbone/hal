module Parser.Scheme.Vars where

import Control.Applicative

import Tokens
import Parser.Base
import {-# SOURCE #-} Parser.Scheme
import Parser.Scheme.Utils
import Parser.Scheme.Atoms


-- | Parse a variable definition
parseDefine :: Parser Scheme
parseDefine = do
  _     <- parseString "("
           >> parseSpaces
           >> parseString "define"
           >> parseSpaces
  ident <- parseIdent
  _ <- parseSpaces
  val   <- parseArg
  _ <- parseSpaces >> parseString ")"
  case ident of
    (Ident name) -> return $ Def name val
    _            -> parserFailure


-- | Parse a procedure definition
parseDefineFn :: Parser Scheme
parseDefineFn = do
  _     <- parseString "("
           >> parseSpaces
           >> parseString "define"
           >> parseSpaces
           >> parseString "("
           >> parseSpaces
  ident <- parseIdent
  _     <- parseSpaces
  fnArgs  <- many parseIdentArg
  _ <- parseSpaces
    >> parseString ")"
    >> parseSpaces
  val   <- parseArg
  _ <- parseSpaces >> parseString ")"
  case ident of
    (Ident name) -> return $ DefFn name (map (\(Ident x) -> x) fnArgs) val False
    _            -> parserFailure


-- | Parse a procedure definition with varargs
parseDefineVfn :: Parser Scheme
parseDefineVfn = do
  _     <- parseString "("
           >> parseSpaces
           >> parseString "define"
           >> parseSpaces
           >> parseString "("
           >> parseSpaces
  ident <- parseIdent
  _     <- parseSpaces
  fnArgs  <- _parseVArg
  _ <- parseSpaces
    >> parseString ")"
    >> parseSpaces
  val   <- parseArg
  _ <- parseSpaces >> parseString ")"
  case ident of
    (Ident name) -> return $ DefFn name (map (\(Ident x) -> x) fnArgs) val True
    _            -> parserFailure


-- | Parse vararg
_parseVArg :: Parser [Scheme]
_parseVArg = do
  _ <- parseString "."
       >> parseSpaces
  _arg <- parseIdentArg
  return [_arg]

-- | Parse let declaration
parseLet :: Parser Scheme
parseLet = do
  _ <- parseString "("
       >> parseSpaces
       >> parseString "let"
       >> parseSpaces
       >> parseString "("
       >> parseSpaces
  vars <- some _parseLetVars
  _ <- parseSpaces
       >> parseString ")"
       >> parseSpaces
  expr <- parseArg
  _ <- parseSpaces >> parseString ")"
  return $ Let vars expr


-- | Parse a single variable in let
_parseLetVars :: Parser (String, Scheme)
_parseLetVars = do
  _ <- parseSpaces
       >> parseString "("
       >> parseSpaces
  name <- parseIdentArg
  _ <- parseSpaces
  val <- parseArg
  _ <- parseSpaces >> parseString ")"
  return $ (unwrapIdent name, val)
  where
    unwrapIdent (Ident val) = val

