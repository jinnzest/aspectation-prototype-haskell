module Environment
  ( getWorkingDir
  ) where

import Control.Monad.Except ((>>=), liftIO, return)
import Data.Text.IO as TIO (putStr, putStrLn)
import Prelude as P (IO, String, ($), (.), putStr)
import SemanticTree (SemanticTree)
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitFailure), exitWith)

getWorkingDir :: IO String
getWorkingDir = getArgsL >>= extractPath >>= printPathL

getArgsL = liftIO getArgs

printPathL = liftIO . printPath

extractPath :: [String] -> IO String
extractPath [arg] = return arg
extractPath _     = do
  TIO.putStrLn
    "The only one command line argument is allowed.\nIt must be a path to the project working directory.\nExample: "
  exitWith $ ExitFailure $ -1

printPath path = do
  TIO.putStr "Project working dir: '"
  P.putStr path
  TIO.putStr "'\n"
  return path
