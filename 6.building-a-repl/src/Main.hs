module Main where

-- import System.Environment
-- import Control.Monad
-- 
-- import Reader
-- import Evaluator
-- import Printer
-- import Error

import REPL

main :: IO ()
main = do
  -- args <- getArgs
  -- readed <- return $ readExpr (args !! 0)
  -- evaled <- return $ readed >>= eval
  -- case evaled of
  --   Left err -> print err
  --   Right val -> print val
  -- runREPL
  repl
