module Eval.Scheme where

import Control.Monad (sequence)
import Tokens
import Utils
import Eval.Error


evalScheme :: Scheme -> [(String, Scheme)] -> (Either EvalError Scheme, [(String, Scheme)])