module Parser.Scheme.Utils where

import Control.Applicative

import Tokens
import Parser.Base
import {-# SOURCE #-} Parser.Scheme
import Parser.Scheme.Atoms

-- | Parse list or procedures args
parseArg :: Parser Scheme
parseArg = do
  _ <- parseSpaces
  res <- parseSchemeArg
  _ <- parseSpaces
  return res



-- | Parse procedures ident args (for procedure definition)
parseIdentArg :: Parser Scheme
parseIdentArg = do
  _ <- parseSpaces
  res <- parseIdent
  _ <- parseSpaces
  return res
