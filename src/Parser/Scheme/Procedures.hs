module Parser.Scheme.Procedures where

import Control.Applicative
import Control.Monad

import Tokens
import Parser.Base
import {-# SOURCE #-} Parser.Scheme
import Parser.Scheme.Utils
import Parser.Scheme.Atoms


-- |
-- | Procedures
-- |

-- | Parse a procedure apply
parseApply :: Parser Scheme
parseApply = do
  _ <- parseString "(" >> parseSpaces
  res <- parseProcedure
  _ <- parseSpaces
  _args <- many parseArg
  _ <- parseSpaces >> parseString ")"
  return $ Apply res _args


-- | Check for reserved keywords
validProcedure :: Scheme -> Parser Bool
validProcedure (Ident name) = if elem name reservedKeywords then
                                parserFailure
                              else
                                return True
  where
    reservedKeywords = ["let", "define", "lambda", "cond", "if", "or", "and", "else"]
validProcedure _ = parserFailure


-- | Parse a custom procedure
parseProcedure :: Parser Scheme
parseProcedure = do
  name <- parseIdent
  _ <- validProcedure name
  return $ Procedure $ Fn (toString name)
  where
    toString (Ident x) = x


-- | Parse a lambda
parseLambda :: Parser Scheme
parseLambda = do
  _     <- parseString "("
           >> parseSpaces
           >> parseString "lambda"
           >> parseSpaces
           >> parseString "("
           >> parseSpaces
  fnArgs  <- many parseIdentArg
  _ <- parseString ")"
       >> parseSpaces
  val   <- parseScheme
  _     <- parseSpaces >> parseString ")"
  return $ AbstractFn "" (map (\(Ident x) -> x) fnArgs) val False


-- | Parse a lambda direct call
parseLambdaCall :: Parser Scheme
parseLambdaCall = do
  _ <- parseString "("
  lambda <- parseLambda
  _ <- parseSpaces
  fnArgs <- many parseArg
  _ <- parseSpaces >> parseString ")"
  return $ Apply (Procedure $ AnonFn lambda) $ fnArgs
