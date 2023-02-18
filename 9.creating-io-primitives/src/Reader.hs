module Reader where

import Text.ParserCombinators.Parsec
import Numeric (readOct, readHex, readFloat)
import Data.Ratio
import Data.Complex
import Control.Monad.Except

import Type
import Error
import Environment

spaces1 :: Parser ()
spaces1 = skipMany1 space

symbol :: Parser Char
symbol = oneOf "~!@#$%^&*-_+=|:<>/?"

parseExpr :: Parser LispVal
parseExpr =  parseCharacter
         <|> parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> parseQuasiQuoted
         <|> parseUnQuoteSplicing
         <|> parseUnQuote
         <|> parseList

escapedChars :: Parser Char
escapedChars  = do
  char '\\'
  x <- oneOf "\\\"nrt"
  return $ case x of
    '\\' -> '\\'
    '\"' -> '\"'
    'n'  -> '\n'
    'r'  -> '\r'
    't'  -> '\t'

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (escapedChars <|> noneOf "\\\"")
  char '"'
  return $ String x

parseCharacter :: Parser LispVal
parseCharacter = do
  try $ string "#\\"
  value <- try $ (string "space" <|> string "newline" <|> string "tab")
       <|> do x <- anyChar
              return [x]
  return . Character $ case value of
    "space" -> ' '
    "newline" -> '\n'
    "tab" -> '\t'
    _ -> value !! 0

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]
parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
  char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]
parseUnQuote :: Parser LispVal
parseUnQuote = do
  char ','
  x <- parseExpr
  return $ List [Atom "unquote", x]
parseUnQuoteSplicing :: Parser LispVal
parseUnQuoteSplicing = do
  try $ string ",@"
  x <- parseExpr
  return $ List [Atom "unquotesplicing", x]

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

parseNumber :: Parser LispVal
parseNumber =  parseHex
           <|> parseOct
           <|> parseBin
           <|> parseComplex
           <|> parseFloat
           <|> parseRatio
           <|> parseDig

readHex' :: String -> Integer
readHex' string = fst $ readHex string !! 0
parseHex :: Parser LispVal
parseHex = try $ string "0x" >> many1 hexDigit >>= return . Number . Integer . readHex'
readOct' :: String -> Integer
readOct' string = fst $ readOct string !! 0
parseOct = try $ string "0o" >> many1 octDigit >>= return . Number . Integer . readHex'
readBin'' :: Integer -> String -> Integer
readBin'' integer "" = integer
readBin'' integer (x:xs) =
  let old = 2 * integer + (if x == '0' then 0 else 1) in
    readBin'' old xs
readBin' :: String -> Integer
readBin' = readBin'' 0
parseBin :: Parser LispVal
parseBin = try $ string "0b" >> many1 (oneOf "01") >>= return . Number . Integer . readBin'
readFloat' :: String -> Double
readFloat' string = fst $ readFloat string !! 0
parseFloat :: Parser LispVal
parseFloat = try $ do
  x <- many1 digit
  char '.'
  y <- many1 digit
  return . Number . Float . readFloat' $ x ++ "." ++ y
parseRatio = try $ do
  x <- many1 digit
  char '/'
  y <- many1 digit
  return . Number . Ratio $ read x % read y
toDouble :: LispVal -> Double
toDouble (Number (Float number)) = realToFrac number
toDouble (Number (Integer number)) = fromIntegral number
parseComplex :: Parser LispVal
parseComplex = try $ do
  x <- parseFloat <|> parseDig
  char '+'
  y <- parseFloat <|> parseDig
  char 'i'
  return . Number . Complex $ toDouble x :+ toDouble y
parseDig :: Parser LispVal
parseDig = many1 digit >>= return . Number . Integer . read

parseList :: Parser LispVal
parseList = do
  char '(' >> spaces
  head <- sepEndBy parseExpr spaces1
  let parseDottedList' = do
        char '.' >> spaces1
        tail <- parseExpr
        spaces >> char ')'
        return $ DottedList head tail
      parseList' = do
        spaces >> char ')'
        return $ List head in
    parseDottedList' <|> parseList'

readORthrow :: Parser a -> String -> ThrowsError a
readORthrow parser input = case parse parser "lisp" input of
  Left error -> throwError $ Parser error
  Right value -> return value

readExpr :: String -> ThrowsError LispVal
-- readExpr input =
--   case parse parseExpr "lisp" input of
--     Left err -> throwError $ Parser err
--     Right val -> return val
readExpr = readORthrow parseExpr
readExprList :: String -> ThrowsError [LispVal]
readExprList = readORthrow $ endBy parseExpr spaces

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList
