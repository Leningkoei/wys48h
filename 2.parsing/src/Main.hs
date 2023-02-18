{-# LANGUAGE LambdaCase #-}

module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

symbol :: Parser Char
symbol = oneOf "~!@#$%^&*-_+=|:<>/?"

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

-- Exercises-2
parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ String x
-- parseString :: Parser LispVal
-- parseString = char '"' >> parseString' where
--   parseString' :: Parser String
--   parseString' = do
--     x <- many $ noneOf "\"\\"
--     anyChar >>= \case
--       '"' -> return x
--       '\\' -> do
--         let f (c, c') = char c >> (x ++) . (c' :) <$> parseString'
--         foldr (\ a b -> f a <|> b) (return "")
--           [ ('"', '"')
--           , ('\'', '\'')
--           , ('\\', '\\')
--           ]
--     -- anyChar >>= \ char -> case char of
--     --   '"' -> return $ String x
--     --   '\\' -> do
--     --     let f (c, c') = char c >> (x ++) . (c' :) <$> parseString'
--     --     -- let f = \ c c' -> char c >> (x ++) . (c' :) <$> parseString'
--     --     foldr (\ a b -> f a <|> b) (return $ String "")
--     --       [ ('"', '"')
--     --       ]

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

-- Exercises-1
-- parseNumber :: Parser LispVal
-- parseNumber = liftM (Number . read) $ many1 digit
parseNumber :: Parser LispVal
parseNumber = many1 digit >>= return . Number . read

spaces :: Parser ()
spaces = skipMany1 space

parseList :: Parser LispVal
parseList = sepBy parseExpr spaces >>= return . List

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseExpr :: Parser LispVal
parseExpr =  parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

readExpr :: String -> String
readExpr input =
  case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)
