module Parser.Error where

-- | Parse errors types (Position + Message)
data Message = Err String
             deriving Show

data ParseError = ParseError !Message

instance Show ParseError where
  show (ParseError msg) = case msg of
    Err str -> "*** ERROR : " ++ str

