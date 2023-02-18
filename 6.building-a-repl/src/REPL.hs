module REPL where

import System.IO
import System.Console.Readline

import Reader
import Evaluator
import Printer
import Error

rep :: String -> String
rep expr = do
  readed <- return $ readExpr expr
  evaled <- return $ readed >>= eval
  handleError evaled

repl :: IO ()
repl = do
  input <- readline "Lisp> " >>= maybe (return "") return
  if input == "quit" || input == ""
    then return ()
    else (putStrLn $ rep input) >> repl

-- flushStr :: String -> IO ()
-- flushStr str = putStr str >> hFlush stdout
-- 
-- readPrompt :: String -> IO String
-- readPrompt prompt = flushStr prompt >> getLine
-- 
-- evalString :: String -> IO String
-- evalString expr = do
--   readed <- return $ readExpr expr
--   evaled <- return $ readed >>= eval
--   case evaled of
--     Left err -> return $ show err
--     Right val -> return $ show val
-- 
-- evalAndPrint :: String -> IO ()
-- evalAndPrint expr = evalString expr >>= putStrLn
-- 
-- -- until-kai :: (String -> Bool) -> IO String -> (String -> IO()) -> IO ()
-- untilKai :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
-- untilKai check prompt action = do
--   result <- prompt
--   if check result
--     then return ()
--     else action result >> untilKai check prompt action
-- 
-- runREPL :: IO ()
-- runREPL = untilKai (== "quit") (readPrompt "Lisp> ") evalAndPrint
