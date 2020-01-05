module Parser.Scheme.Atoms where

import Control.Applicative
import Data.Char

import Tokens
import Parser.Base


-- |
-- | Atoms
-- |

-- | Parse all atoms
parseAtoms :: Parser Scheme
parseAtoms = parseSchemeChar
             <|> parseSchemeNumber
             <|> parseSchemeString
             <|> parseIdent


-- | Parse atomic identifier symbol
parseIdentSymbol :: Parser Char
parseIdentSymbol = parseOneOf "!#$%&+-*/|:><?=_^@~"


-- | Parse atomic identifier
parseIdent :: Parser Scheme
parseIdent = do
  symbol <- parseIdentSymbol <|> parseAlpha
  name <- many $ parseDigit <|> parseAlpha <|> parseIdentSymbol
  return $ case symbol:name of
    "#t" -> Bool True
    "#f" -> Bool False
    ident -> Ident ident


-- | Parse a number
parseSchemeNumber :: Parser Scheme
parseSchemeNumber = Number <$> parseNumber


-- | Parse a scheme char
parseSchemeChar :: Parser Scheme
parseSchemeChar = (Char <$> (parseChar '#' >> parseEscapeChar))
                  <|> (Char <$> (parseString "#\\"
                                 >> parseOne))


-- | Parse escaped char (like \n \r...)
parseEscapeChar :: Parser Char
parseEscapeChar = do
  _ <- parseChar '\\'
  c <- (parseString "alarm" >> return "\\a")
       <|> (parseString "backspace" >> return "\\b")
       <|> (parseString "tab" >> return "\\t")
       <|> (parseString "newline" >> return "\\n")
       <|> (parseString "vtab" >> return "\\v")
       <|> (parseString "page" >> return "\\f")
       <|> (parseString "return" >> return "\\r")
       <|> (parseString "space" >> return " ")
       <|> parserFailure
  res <- do case readLitChar c of
              [(char, _)] -> return char
              _ -> parserFailure
  return res


-- | Parse escaped char (like \n \r...)
parseEscape :: Parser Char
parseEscape = do
  _ <- parseChar '\\'
  c <- ((parseString "a") >> return "\\a")
       <|> (parseString "b" >> return "\\b")
       <|> (parseString "t" >> return "\\t")
       <|> (parseString "n" >> return "\\n")
       <|> (parseString "v" >> return "\\v")
       <|> (parseString "f" >> return "\\f")
       <|> (parseString "r" >> return "\\r")
       <|> (parseString " " >> return " ")
       <|> parserFailure
  res <- do case readLitChar c of
              [(char, _)] -> return char
              _ -> parserFailure
  return res


-- | Parse a string
parseSchemeString :: Parser Scheme
parseSchemeString = do
  _   <- parseString quote
  val <- many $ parseEscape <|> parseNoneOf quote
  _   <- parseString quote
  return $ String val
  where
    quote = "\""
