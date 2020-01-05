module Eval.Scheme.Vars where

import Tokens
import Eval.Error


-- | Get a variable value
getValue :: String -> [(String, Scheme)] -> Either EvalError Scheme
getValue name [] = Left $ throw $ "Variable " ++ name ++ " is not bound."
getValue name ((varname, val):xs) = if varname == name
                                    then Right val
                                    else getValue name xs


