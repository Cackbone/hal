module Files where

import System.Directory

import Utils
import Tokens
import Lib


-- | Interpret multiple files
filesInterpreter :: [String] -> (String -> [(String, Scheme)] -> IO [(String, Scheme)]) -> [(String, Scheme)] -> IO [(String, Scheme)]
filesInterpreter files fn vars = do
  _ <- checkFiles files >> return []
  content <- sequence $ map (\x -> readFile x) files
  fn (concat content) vars


-- | Check if all files exists
checkFiles :: [String] -> IO [(String, Scheme)]
checkFiles (x:xs) = do
  exist <- doesFileExist x
  handle exist
  where
    handle exist
      | exist && length xs == 0 = return []
      | exist = checkFiles xs >> return []
      | otherwise = (exitWithErr $ "*** ERROR : " ++ x ++ " no such file") >> return []
