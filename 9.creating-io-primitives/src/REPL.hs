module REPL where

import System.IO
import System.Console.Readline

import Type
import Reader
import Evaluator
import Printer
import Error
import Environment

rep :: Env -> String -> IO String
rep env expr = do
  readed <- return . liftThrows $ readExpr expr
  evaled <- return $ readed >>= eval env
  runIOThrows evaled

repl :: Env -> IO ()
repl env = do
  input <- readline "Lisp> " >>= maybe (return "") return
  if input == "quit" || input == ""
    then return ()
    else do
      result <- rep env input
      putStrLn result >> repl env
