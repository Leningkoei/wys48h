module Main where

import System.Environment
import Control.Monad

import Reader
import Evaluator
import Printer
import Error

main :: IO ()
-- main = getArgs >>= print . eval . readExpr . head
main = do
  -- args <- getArgs
  -- evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
  -- putStrLn $ extractValue $ trapError evaled
  -- args <- getArgs
  -- evaled <- return $ readExpr (args !! 0) >>= eval
  -- print $ extractValue $ trapError evaled
  args <- getArgs
  readed <- return $ readExpr (args !! 0)
  evaled <- return $ readed >>= eval
  case evaled of
    Left err -> print err
    Right val -> print val
