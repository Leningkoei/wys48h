module Main where

import Control.Monad.Except

import REPL
import Environment

main :: IO ()
main = nullEnv >>= repl
