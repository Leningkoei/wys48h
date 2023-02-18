module Evaluator where

import Control.Monad.Except

import Type
import Error

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number (Integer n)) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op [parameter] = throwError $ NumArgs 2 [parameter]
numericBinop op parameters = mapM unpackNum parameters >>=
  return . Number . Integer . foldl1 op

symbolp :: LispVal -> LispVal
symbolp (Atom _) = Bool True
symbolp _ = Bool False
numberp :: LispVal -> LispVal
numberp (Number _) = Bool True
numberp _ = Bool False
stringp :: LispVal -> LispVal
stringp (String _) = Bool True
stringp _ = Bool False
boolp :: LispVal -> LispVal
boolp (Bool _) = Bool True
boolp _ = Bool False
listp :: LispVal -> LispVal
listp (List _) = Bool True
listp _ = Bool False

symbol2string :: LispVal -> LispVal
symbol2string (Atom symbol) = String symbol
symbol2string _ = String ""
string2symbol :: LispVal -> LispVal
string2symbol (String string) = Atom string
string2symbol _ = Atom ""

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp op [parameter] = return $ op parameter
unaryOp op parameters = throwError $ NumArgs 1 parameters

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [ ("+", numericBinop (+))
             , ("-", numericBinop (-))
             , ("*", numericBinop (*))
             , ("/", numericBinop div)
             , ("mod", numericBinop mod)
             , ("quotient", numericBinop quot)
             , ("remainder", numericBinop rem)
             , ("symbol?", unaryOp symbolp)
             , ("number?", unaryOp numberp)
             , ("string?", unaryOp stringp)
             , ("bool?", unaryOp boolp)
             , ("list?", unaryOp listp)
             , ("symbol->string", unaryOp symbol2string)
             , ("string->symbol", unaryOp string2symbol)
             ]

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe
  (throwError $ NotFunction "Unrecognized primitive function args" func)
  ($ args) $ lookup func primitives

eval :: LispVal -> ThrowsError LispVal
eval (String val) = return $ String val
eval (Character val) = return $ Character val
eval (Number val) = return $ Number val
eval (Bool val) = return $ Bool val
-- evaluation of quoted exp: take off the val
eval (List [Atom "quote", val]) = return $ val

eval (List (Atom func : args)) = mapM eval args >>= apply func
