module LibsLoader where

import System.IO
import System.Directory
import System.Environment
import Control.Exception


import Tokens
import Exec
import Utils
import Files


-- | Load libraries before interpreting the program
libsLoader :: [String] -> IO [(String, Scheme)]
libsLoader [] = return []
libsLoader libs = filesInterpreter libs execLibs []


-- | Allowed scheme file extensions for libraries
schemeExts :: [String]
schemeExts = [".scm", ".sps", ".sls", ".sld", ".lisp"]

-- | Load builtins
stdLoader :: IO [(String, Scheme)]
stdLoader = do
  folderPath <- stdFolder
  exist <- doesDirectoryExist folderPath
  std <- if exist && (length folderPath /= 0)
    then getDirectoryContents folderPath
    else return []
  if exist && (length std /= 0)
    then libsLoader $ formatLibs (filterLibs std) folderPath
    else return []
  where
    checkDotfiles file = file !! 0 /= '.'
    checkExt file = (or . map (\x -> x == (reverse . take (length x) . reverse) file)) schemeExts
    filterLibs libs = filter (\x -> checkExt x && checkDotfiles x) libs
    formatLibs libs folder = map (\x -> folder ++ "/" ++ x) libs
    stdFolder = catch (getEnv "HAL_STD") errHandler
    errHandler :: IOError -> IO String
    errHandler = (\_ -> return [])



