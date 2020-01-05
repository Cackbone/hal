module Parser.Scheme.Lists where

import Control.Applicative
import Control.Monad

import Tokens
import Utils
import Parser.Base
import {-# SOURCE #-} Parser.Scheme
import Parser.Scheme.Utils


-- |
-- | Lists
-- |

-- | Parse a list
parseList :: Parser Scheme
parseList = do
  _ <- parseSpaces
       >> parseString "("
       >> parseSpaces
  cells <- many parseArg
  _ <- parseSpaces >> parseString ")"
  liftM List $ do { return $ arrayToList cells }


-- | Parse ConsCell
parseConsCell :: Parser Scheme
parseConsCell =  do
  _ <- parseString "(" >> parseSpaces
  car <- parseSchemeArg
  _ <- parseSomeSpaces >> parseString "." >> parseSomeSpaces
  cdr <- parseSchemeArg
  _ <- parseSpaces >> parseString ")"
  return $ ConsCell $ ConsCellData car cdr
