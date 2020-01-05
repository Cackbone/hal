module Eval.Scheme.Utils where

import Tokens
import Utils
import {-# SOURCE #-} Eval.Scheme
import Eval.Error


-- |
-- | Conditions
-- |


-- | Check if list is empty
isEmptyList :: Scheme -> Bool
isEmptyList scheme = case scheme of
  List(ConsCellData Void Void) -> True
  _ -> False


-- | Check if 2 values are equals
isEqual :: [Scheme] -> Bool
isEqual (Number left : Number right : []) = left == right
isEqual (Bool left : Bool right : []) = left == right
isEqual (List left : List right : []) = isEmptyList (List left)
                                        && isEmptyList (List right)
isEqual (Ident left : Ident right : []) = show left == show right
isEqual (String [] : String [] : []) = True
isEqual _ = False


isEquiv :: [Scheme] -> Bool
isEquiv (AbstractFn "" _ _ _ : AbstractFn "" _ _ _ : []) = False
isEquiv (AbstractFn f1 _ _ _ : AbstractFn f2 _ _ _ : []) = f1 == f2
isEquiv (x : y : []) = show x == show y
isEquiv xs = isEqual xs


-- | Get type of an expression
getType :: Scheme -> String
getType expr = case expr of
  Char _             -> "char"
  String _           -> "string"
  Number _           -> "number"
  Bool _             -> "boolean"
  ConsCell _         -> "pair"
  List _             -> "list"
  Procedure _        -> "procedure"
  AbstractFn _ _ _ _ -> "procedure"
  Void               -> "void"
  Ident _            -> "symbol"


-- | Num equal
numEqual :: [Integer] -> Bool
numEqual (left : right : []) = left == right
numEqual _ = False


-- | String equals
strEquals :: [String] -> Bool
strEquals [] = False
strEquals (_:[]) = True
strEquals (s1:s2:str) = s1 == s2 && strEquals (s2:str)


-- |
-- | Cast scheme
-- |

-- | Convert scheme to integer
intFromScheme :: Either EvalError Scheme -> Either EvalError Integer
intFromScheme s = case s of
  Right x -> case x of
    Number y -> Right y
    invalid -> Left $ throw (show invalid ++ " is not a number")
  Left err -> Left err


-- | Convert scheme to integer
stringFromScheme :: Either EvalError Scheme -> Either EvalError String
stringFromScheme s = case s of
  Right x -> case x of
    String y -> Right y
    invalid -> Left $ throw (show invalid ++ " is not a string")
  Left err -> Left err


-- | Convert scheme to integer
listFromScheme :: Either EvalError Scheme -> Either EvalError [Scheme]
listFromScheme s = case s of
  Right x -> case x of
    List y -> Right $ listToArray y
    invalid -> Left $ throw (show invalid ++ " is not a list")
  Left err -> Left err


-- | Convert scheme to integer
identNameFromScheme :: Either EvalError Scheme -> Either EvalError String
identNameFromScheme s = case s of
  Right x -> case x of
    Ident y -> Right y
    invalid -> Left $ throw (show invalid ++ " is not a symbol")
  Left err -> Left err



-- | Convert scheme to conscell
consFromScheme :: Either EvalError Scheme -> Either EvalError ConsCellData
consFromScheme s = case s of
  Right x -> case x of
    ConsCell y -> Right y
    List y -> Right y
    invalid -> Left $ throw (show invalid ++ " is not a pair")
  Left err -> Left err



-- |
-- | Eval args
-- |

-- | Eval arguments of a procedure
evalArgsCast :: SchemeProcedure
  -> [Scheme]
  -> (Either EvalError Scheme -> Either EvalError a)
  -> Int
  -> [(String, Scheme)]
  -> Either EvalError [a]
evalArgsCast p _args castFn argCount vars
  | argCount == length _args = sequence $ map castFn $ map (\x -> fst $ evalScheme x vars) _args
  | otherwise = Left $ throw $ "incorrect argument count in call " ++ (show $ Apply (Procedure p) _args)



-- | Eval arguments of a procedure
evalArgs :: SchemeProcedure -> [Scheme] -> Int -> [(String, Scheme)] -> Either EvalError [Scheme]
evalArgs p _args argCount vars
  | argCount == length _args = sequence $ map (\x -> fst $ evalScheme x vars) _args
  | otherwise = Left $ throw $ "incorrect argument count in call " ++ (show $ Apply (Procedure p) _args)



-- | Check if sequence is decreasing
isDecreasing :: [Integer] -> Bool
isDecreasing [] = True
isDecreasing [_] = True
isDecreasing (x:[y]) = x > y
isDecreasing (x:(y:xs)) = (x > y) && isDecreasing (y:xs)


-- | Check if sequence is increasing
isIncreasing :: [Integer] -> Bool
isIncreasing [] = True
isIncreasing [_] = True
isIncreasing (x:[y]) = x < y
isIncreasing (x:(y:xs)) = (x < y) && isIncreasing (y:xs)


-- | Get last element in conscell
getLastCell :: ConsCellData -> Scheme
getLastCell (ConsCellData car Void) = car
getLastCell (ConsCellData _ (ConsCell x)) = getLastCell x
getLastCell (ConsCellData _ cdr) = cdr
