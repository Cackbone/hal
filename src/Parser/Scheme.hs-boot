module Parser.Scheme (
       parseScheme,
       parseSchemeArg) where

import Control.Applicative
import Control.Monad

import Tokens
import Parser.Base


-- | Parse a scheme expression
parseScheme :: Parser Scheme

parseSchemeArg :: Parser Scheme

-- | Parse a scheme quote
parseQuote :: Parser Scheme
