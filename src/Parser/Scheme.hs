module Parser.Scheme (
  parseScheme,
  parseSchemeArg) where

import Control.Applicative
import Control.Monad

import Tokens
import Parser.Base
import Parser.Scheme.Procedures
import Parser.Scheme.Statements
import Parser.Scheme.Lists
import Parser.Scheme.Atoms
import Parser.Scheme.Vars


-- | Parse a scheme expression
parseScheme :: Parser Scheme
parseScheme = do
  _ <- parseSpaces
  expr <- parseSchemeArg
  _ <- parseSpaces
  return expr


-- | Parse a scheme expression (without procedures)
parseSchemeArg :: Parser Scheme
parseSchemeArg = parseQuote
                 <|> parseLet
                 <|> parseStatements
                 <|> parseAtoms
                 <|> parseProcedure
                 <|> parseLambdaCall
                 <|> parseLambda
                 <|> parseDefineVfn
                 <|> parseDefineFn
                 <|> parseDefine
                 <|> parseApply
                 <|> parseConsCell
                 <|> parseList


-- | Parse a scheme quote
parseQuote :: Parser Scheme
parseQuote = full <|> short
  where
    full = do
      _ <- parseString "(" >> parseSpaces
      _ <- parseString "quote" >> parseSpaces
      res <- parseScheme
      _ <- parseSpaces >> parseString ")"
      return $ Quote res
    short = Quote <$> (parseString "'"
                       >> parseSpaces
                       >> parseSchemeArg)


