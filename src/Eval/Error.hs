module Eval.Error where

-- | Eval error type
data Message = Err String
             deriving Show


data EvalError = EvalError !Message


instance Show EvalError where
  show (EvalError msg) = case msg of
    Err str -> "*** ERROR : " ++ str


-- | Helper function to throw an error
throw :: String -> EvalError
throw str = EvalError $ Err str
