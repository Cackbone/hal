module Eval.Scheme where

import Control.Monad (sequence)
import Tokens
import Utils
import Eval.Error
import Eval.Scheme.Utils
import Eval.Scheme.Procedures
import Eval.Scheme.Vars


-- | Eval scheme
evalScheme :: Scheme -> [(String, Scheme)] -> (Either EvalError Scheme, [(String, Scheme)])


-- | Eval an apply procedure
evalScheme (Apply (Procedure p) _args) vars = evalProcedure p _args vars
evalScheme (Apply (AbstractFn "" names val isVar) _args) vars = evalProcedure (AnonFn $ AbstractFn "" names val isVar) _args vars
evalScheme (Apply (AbstractFn name _ _ _) _args) vars = evalProcedure (Fn name) _args vars
evalScheme (Apply x _args) vars = (Left $ throw $ "attempt to apply non-procedure " ++ (show x), vars)


-- | Eval raw fn procedure
evalScheme (Procedure (Fn fname)) vars = case getValue fname vars of
  Right _ -> (Right $ Procedure $ Fn fname, vars)
  Left err -> if isPrimitive $ Fn fname
    then (Right $ Procedure $ Fn fname, vars)
    else (Left err, vars)


-- | Eval a raw list (without quote)
evalScheme (List (ConsCellData x _)) vars = (Left $ throw $ "attempt to apply non-procedure " ++ (show x), vars)


-- | Eval a raw conscell (without quote)
evalScheme (ConsCell x) vars = (Left $ throw $ "invalid syntax " ++ (show $ ConsCell x), vars)


-- | Eval a scheme quote
evalScheme (Quote x) vars = (Right x, vars)


-- | Eval a variable definition
evalScheme (Def name val) vars = case evalScheme val vars of
  (Right res, resVars) -> (Right $ Ident name, ((name, res):resVars))
  x -> x

-- | Eval a defined function
evalScheme (DefFn name args val isVar) vars = (Right $ Ident name, ((name, AbstractFn name args val isVar):vars))


-- | Eval a variable
evalScheme (Ident name) vars = case getValue name vars of
  Right res -> case res of
    AbstractFn "" names fn isVar -> (Right $ AbstractFn name names fn isVar, vars)
    x -> (Right x, vars)
  Left err  -> if isPrimitive $ Fn name
               then (Right $ Procedure $ Fn name, vars)
               else (Left err, vars)


-- | Eval a let expression
evalScheme (Let locals expr) vars = execLet
  where
    execLet = case evalLocals of
      Right resLocals -> (fst $ evalScheme expr (resLocals ++ vars), vars)
      Left e -> (Left e, vars)
    evalLocals :: Either EvalError [(String, Scheme)]
    evalLocals = sequence $ map (\(name, e) -> case evalScheme e vars of
                                    (Right res, _) -> Right (name, res)
                                    (Left e, _) -> Left e
                                ) locals

-- | Eval a cond statement
evalScheme (Cond ((List (ConsCellData car cdr)):xs)) vars = case evalScheme car vars of
  (Right (Bool False), resVars) -> if length xs == 0
    then (Right Void, resVars)
    else evalScheme (Cond xs) resVars
  (Right res, resVars) -> case cdr of
    ConsCell x -> evalScheme (getLastCell x) resVars
    Void -> (Right res, resVars)
    x -> evalScheme x resVars
  (Left e, _) -> (Left e, vars)


-- | Eval if
evalScheme (If cond _then _else) vars = case evalScheme cond vars of
  (Right (Bool False), resVars) -> evalScheme _else resVars
  (Right x, resVars) -> evalScheme _then resVars
  (Left e, resVars) -> (Left e, resVars)


-- | Eval and
evalScheme (And []) vars = (Right $ Bool True, vars)
evalScheme (And (x:[])) vars = evalScheme x vars
evalScheme (And (x:xs)) vars = case evalScheme x vars of
  (Right (Bool False), resVars) -> (Right $ Bool False, resVars)
  (Right x, resVars) -> evalScheme (And xs) resVars
  x -> x


-- | Eval or
evalScheme (Or []) vars = (Right $ Bool False, vars)
evalScheme (Or (x:[])) vars = evalScheme x vars
evalScheme (Or (x:xs)) vars = case evalScheme x vars of
  (Right (Bool False), resVars) -> evalScheme (Or xs) resVars
  (Right x, resVars) -> (Right x, resVars)
  x -> x


-- | Default evaluation
evalScheme x vars = (Right x, vars)


