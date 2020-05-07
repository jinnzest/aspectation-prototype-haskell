module Main where

import Environment
import ML

main :: IO ()
main = do
  dir <- getWorkingDir
  runML dir
