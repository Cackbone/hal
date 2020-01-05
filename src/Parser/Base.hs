module Parser.Base where

import Control.Monad
import Control.Applicative

import Parser.Error


-- | Apply a Parser 'p' for type 'a' on a string 's'
-- | and return a value of type 'Maybe a'
applyParser :: Parser a -> String -> Either ParseError a
applyParser parser str = case parse parser str of
  [(x, [])] -> Right x
  _  -> Left $ ParseError $ Err $ "error while parsing expression " ++ str


-- |
-- | Parser type definition
-- |

-- | A Parser type with a parse function to build a AST
newtype Parser a = Parser { parse :: String -> [(a, String)] }


-- | Parser Monad
instance Monad Parser where
  return = parserPure
  (>>=) = bindParser


-- | Parser Monad plus
instance MonadPlus Parser where
  mzero = parserFailure
  -- | Apply two parsers on same stream and concat
  mplus p1 p2 = Parser $ \str -> parse p1 str ++ parse p2 str


-- | Parser functor
instance Functor Parser where
  fmap fn (Parser a) = Parser $ \s -> [(fn x, y) | (x, y) <- a s]


-- | Parser applicative
instance Applicative Parser where
  pure = parserPure
  (Parser p1) <*> (Parser p2) = Parser $ \str ->
    [(fn x, str2) | (fn, str1) <- p1 str, (x, str2) <- p2 str1]


-- | Parser alternative
instance Alternative Parser where
  empty = parserFailure
  -- | Parser "or" operator (if p1 is mzero take p2 else
  (<|>) p1 p2 = Parser $ \str ->
    case parse p1 str of
      [] -> parse p2 str
      x -> x


-- | Wrap a type 'a' in Parser
parserPure :: a -> Parser a
parserPure a = Parser $ \str -> [(a, str)]


-- | Take parse function and build it over a second
-- | Apply parser 1 then apply parser 2
bindParser :: Parser a -> (a -> Parser b) -> Parser b
bindParser parser fn = Parser $ \str ->
  concatMap (\(a, str2) -> parse (fn a) str2)
  $ parse parser str


-- | Returned when parser failed
parserFailure :: Parser a
parserFailure = Parser $ \_ -> []



-- |
-- | Parser helpers
-- |


-- | Extract single char of the stream and transform it in a Parser Char
-- | Parser char (char, rest)
parseOne :: Parser Char
parseOne = Parser $ \str ->
  case str of
    []     -> []
    (c:cs) -> [(c,cs)]


-- | Check if char in stream satisfy the checker parse char otherwise return failure
check :: (Char -> Bool) -> Parser Char
check checker = bindParser parseOne $ \char ->
  if checker char then
    parserPure char
  else
    parserFailure


-- | Parse one of char in string
parseOneOf :: String -> Parser Char
parseOneOf str = check $ flip elem str


-- | Parse char if not in string
parseNoneOf :: String -> Parser Char
parseNoneOf str = check $ \c -> not $ elem c str


-- | Chained parse to handle priorities
chain :: Parser a -> Parser (a -> a -> a) -> Parser a
chain p1 p2 = do { x <- p1; left x }
  where left x =
          (do p <- p2
              y   <- p1
              left $ p x y)
          <|> return x


-- | Parse char
parseChar :: Char -> Parser Char
parseChar char = check (char ==)


-- | Parse a string
parseString :: String -> Parser String
parseString (x:xs) = do {
  _ <- parseChar x;
  _ <- parseString xs;
  return (x:xs)
}
parseString [] = return []


-- | Parse a digit
parseDigit :: Parser Char
parseDigit = parseOneOf ['0'..'9']


-- | Parse a letter
parseAlpha :: Parser Char
parseAlpha = parseOneOf $ ['a'..'z'] ++ ['A'..'Z']


-- | Parse a number
parseNumber :: Parser Integer
parseNumber = do
  sign <- parseString "-" <|> return []
  int <- some parseDigit
  return $ read $ sign ++ int


-- | Parse whitespaces
parseSomeSpaces :: Parser String
parseSomeSpaces = some $ parseOneOf " \n\r"


-- | Parse whitespaces
parseSpaces :: Parser String
parseSpaces = many $ parseOneOf " \n\r"
