module Eval.Scheme.Procedures (evalProcedure, isPrimitive) where

import Control.Applicative

import Tokens
import Utils
import Eval.Error
import {-# SOURCE #-} Eval.Scheme
import Eval.Scheme.Utils
import Eval.Scheme.Vars


-- | Eval a scheme procedure
evalProcedure :: SchemeProcedure -> [Scheme] -> [(String, Scheme)] -> (Either EvalError Scheme, [(String, Scheme)])
evalProcedure p _args vars = case p of
  Fn _  -> evalFns p _args vars
  AnonFn fn -> (evalAnonFn _args fn vars, vars)


-- | Eval primitives and custom functions
evalFns :: SchemeProcedure -> [Scheme] -> [(String, Scheme)] -> (Either EvalError Scheme, [(String, Scheme)])
evalFns p _args vars = case p of
  Fn "apply"         -> evalApply p _args vars
  Fn "eval"          -> evalEval p _args vars
  Fn "+"             -> (evalOperation (\x -> Number $ sum x) p _args (length _args) vars, vars)
  Fn "-"             -> (evalOperation (
                        \x -> Number $ if (length _args == 1)
                                       then -(x !! 0)
                                       else foldl (-) (2 * x !! 0) x)
                      p _args (length _args) vars, vars)
  Fn "*"             -> (evalOperation (\x -> Number $ product x) p _args (length _args) vars, vars)
  Fn "div"           -> (evalOperation (\x -> Number $ x !! 0 `quot` x !! 1) p _args (length _args) vars, vars)
  Fn "mod"           -> (evalOperation (\x -> Number $ x !! 0 `mod` x !! 1) p _args (length _args) vars, vars)
  Fn "<"             -> (evalOperation (\x -> Bool $ isIncreasing x) p _args (length _args) vars, vars)
  Fn  ">"            -> (evalOperation (\x -> Bool $ isDecreasing x) p _args (length _args) vars, vars)
  Fn "cons"          -> (evalCons p _args 2 vars, vars)
  Fn "car"           -> (evalCar p _args 1 vars, vars)
  Fn "cdr"           -> (evalCdr p _args 1 vars, vars)
  Fn "eq?"           -> (evalCond isEqual (evalArgs p _args 2 vars) vars, vars)
  Fn "eqv?"           -> (evalCond isEquiv (evalArgs p _args 2 vars) vars, vars)
  Fn "="             -> (evalCond numEqual (evalArgsCast p _args intFromScheme 2 vars) vars, vars)
  Fn "string=?"      -> (evalCond strEquals (evalArgsCast p _args stringFromScheme (length _args) vars) vars, vars)
  Fn "string->symbol" -> (execProcedure p
                          (\x -> evalArgsCast x _args stringFromScheme 1 vars)
                          (\x -> Ident $ x !! 0), vars)
  Fn "symbol->string" -> (execProcedure p
                          (\x -> evalArgsCast x _args identNameFromScheme 1 vars)
                          (\x -> String $ x !! 0), vars)
  Fn "string->list" -> (execProcedure p
                          (\x -> evalArgsCast x _args stringFromScheme 1 vars)
                          (\x -> List $ arrayToList $ map Char $ x !! 0), vars)
  Fn "string->number" -> (execProcedure p
                          (\x -> evalArgsCast x _args stringFromScheme 1 vars)
                          (\x -> Number $ read $ x !! 0), vars)
  Fn "list->string" -> (execProcedure p
                         (\x -> evalArgsCast x _args listFromScheme 1 vars)
                         (\x -> arrayToString $ x !! 0), vars)
  Fn "type?"         -> case evalArgs p _args 1 vars of
    Right x -> (Right $ String $ getType $ x !! 0, vars)
    Left e  -> (Left e, vars)
  Fn fname  -> (evalFn fname _args vars, vars)


-- | Convert an char array to string
arrayToString :: [Scheme] -> Scheme
arrayToString list = String $ map castFn list
  where
    castFn (Char c) = c
    castFn Void = '\0'


-- | Evalute 'apply' procedure
evalApply :: SchemeProcedure -> [Scheme] -> [(String, Scheme)] -> (Either EvalError Scheme, [(String, Scheme)])
evalApply p _args vars = case eArgs of
  Right arr -> evalScheme (Apply (arr !! 0 !! 0 !! 0) (map quotifier $ arr !! 1 !! 0)) vars
  Left e -> (Left e, vars)
  where
    eArgs = sequence [sequence [_proc], lst]
    lst = evalArgsCast p [_args !! 1] listFromScheme 1 vars
    _proc = evalArgs p [_args !! 0] 1 vars
    quotifier (List x) = Quote $ List x
    quotifier (ConsCell x) = Quote $ ConsCell x
    quotifier x = x


-- | Evaluate 'eval' procedure
evalEval :: SchemeProcedure -> [Scheme] -> [(String, Scheme)] -> (Either EvalError Scheme, [(String, Scheme)])
evalEval _ [Quote expr] vars = evalScheme expr vars
evalEval p _args vars
  | length _args /= 1 = (Left $ throw $ "incorrect argument count in call " ++ show p, vars)
  | otherwise = evalScheme (_args !! 0) vars


-- | Check if primitives
isPrimitive :: SchemeProcedure -> Bool
isPrimitive (Fn name) = elem name ["+", "-", "*", "div",
                                   "mod", "<", ">", "cons",
                                   "car", "cdr", "eq?",
                                   "=", "string=?",
                                   "string->symbol",
                                   "symbol->string",
                                   "string->list",
                                   "eqv?", "apply",
                                   "string->number"]
isPrimitive (AnonFn _) = False


-- | Eval a defined function
evalFn :: String -> [Scheme] -> [(String, Scheme)] -> Either EvalError Scheme
evalFn fname fnArgs vars = case getValue fname vars of
  Right (AbstractFn _ names fn isVar) -> case varsToVargs fnArgs isVar of
    Right vargs -> case evalArgs (Fn fname) vargs (length names) vars of
      Right resArgs -> fst $ evalScheme fn $ (argsToVars names resArgs) ++ vars
      Left err -> Left err
    Left err -> Left err
  Right (Procedure p) -> fst $ evalProcedure p fnArgs vars
  Right (f) -> Left $ throw $ show f
  Left err -> Left err
  where
      argsToVars names _args = getZipList $ (,) <$> ZipList names <*> ZipList _args
      varsToVargs _args _isvar = if _isvar
                                then sequence [evalVargs _args vars]
                                else Right _args


-- | Eval variadics args
evalVargs :: [Scheme] -> [(String, Scheme)] -> Either EvalError Scheme
evalVargs vargs vars = case _evals of
  Right res -> Right $ Quote $ List $ arrayToList res
  Left err -> Left err
  where
    _evals = sequence $ map (\x -> fst $ evalScheme x vars) vargs


-- | Eval anonymous functions
evalAnonFn :: [Scheme] -> Scheme -> [(String, Scheme)] -> Either EvalError Scheme
evalAnonFn fArgs (AbstractFn _ names fn _) vars = case evalArgs (AnonFn anonFn) fArgs (length names) vars of
  Right resArgs -> fst $ evalScheme fn $ (argsToVars resArgs) ++ vars
  Left err -> Left err
 where
   anonFn = AbstractFn "" names fn False
   argsToVars _args = getZipList $ (,) <$> ZipList names <*> ZipList _args


-- | Eval cons
evalCons :: SchemeProcedure -> [Scheme] -> Int -> [(String, Scheme)] -> Either EvalError Scheme
evalCons p _args argCount vars = case evalArgs p _args argCount vars of
  Right x -> case (x !! 1) of
    List y -> Right $ List $ ConsCellData (x !! 0) $ ConsCell y
    _ -> Right $ ConsCell $ ConsCellData (x !! 0) (x !! 1)
  Left err -> Left err

-- | Eval car
evalCar :: SchemeProcedure -> [Scheme] -> Int -> [(String, Scheme)] -> Either EvalError Scheme
evalCar p _args argCount vars = case evalArgsCast p _args consFromScheme argCount vars of
  Right ((ConsCellData Void _):_) -> Left $ throw "() is not a pair"
  Right ((ConsCellData x _):_) -> Right x
  Left err -> Left err


-- | Eval cdr
evalCdr :: SchemeProcedure -> [Scheme] -> Int -> [(String, Scheme)] -> Either EvalError Scheme
evalCdr p _args argCount vars = case evalArgsCast p _args consFromScheme argCount vars of
  Right ((ConsCellData _ y):_) -> case y of
    Void -> Right $ List $ ConsCellData Void Void
    ConsCell(x) -> Right $ List x
    x -> Right x
  Left err -> Left err


-- | Eval 'unary' condition
evalUnaryCond :: (Scheme -> Bool) -> SchemeProcedure -> [Scheme] -> [(String, Scheme)] -> Either EvalError Scheme
evalUnaryCond condFn p _args vars = case evalArgs p _args 1 vars of
  Right res -> Right $ Bool $ condFn (res !! 0)
  Left err -> Left err


-- | Eval condition
evalCond :: ([a] -> Bool) -> Either EvalError [a] -> [(String, Scheme)] -> Either EvalError Scheme
evalCond condFn _evalArgs vars = case _evalArgs of
  Right res -> Right $ Bool $ condFn res
  Left err -> Left err


-- | Eval a scheme operation
evalOperation :: ([Integer] -> Scheme) -> SchemeProcedure -> [Scheme] -> Int -> [(String, Scheme)] -> Either EvalError Scheme
evalOperation fn p _args argCount vars = execProcedure p (\x -> evalArgsCast x _args intFromScheme argCount vars) fn


-- | Execute a procedure
execProcedure :: SchemeProcedure
  -> (SchemeProcedure -> Either EvalError [a])
  -> ([a] -> Scheme)
  -> Either EvalError Scheme
execProcedure p evalFn fn = case evalFn p of
  Right res -> Right $ fn res
  Left err -> Left err
