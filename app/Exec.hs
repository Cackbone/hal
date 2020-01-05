module Exec where

import System.IO

import Lib
import Tokens
import Utils



-- | Exec scheme exprs and print last line result
execExpr :: String -> [(String, Scheme)] -> IO [(String, Scheme)]
execExpr [] vars = return vars
execExpr exprs vars = case execScheme exprs vars of
  (Right Void, resVars) -> return resVars
  (Right x, resVars)    -> (putStrLn $ show x) >> return resVars
  (Left err, resVars)   -> (hPutStrLn stderr $ show err) >> return resVars


-- | Exec scheme exprs and print last line result
execExprs :: String -> [(String, Scheme)] -> IO [(String, Scheme)]
execExprs [] vars = return vars
execExprs exprs vars = case execScheme exprs vars of
  (Right Void, resVars) -> return resVars
  (Right x, resVars)    -> (putStrLn $ show x) >> return resVars
  (Left err, resVars)   -> exitWithErr $ show err


-- | Exec scheme exprs and print last line result
execLibs :: String -> [(String, Scheme)] -> IO [(String, Scheme)]
execLibs [] vars = return vars
execLibs exprs vars = case execScheme exprs vars of
  (Right _, resVars) -> return resVars
  (Left err, resVars)   -> (hPutStrLn stderr $ show err) >> return resVars
