module Main where

import System.Environment

import REPL
import Primitive

main :: IO ()
main = do
  args <- getArgs
  if null args
    then primitiveBindedEnv >>= repl
    else do
      result <- primitiveBindedEnv >>= (`rep` (args !! 0))
      putStrLn result
