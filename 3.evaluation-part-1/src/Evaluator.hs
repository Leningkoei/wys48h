module Evaluator where

import Type

unpackNum :: LispVal -> Integer
unpackNum (Number (Integer n)) = n

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op parameters = Number . Integer $ foldl1 op $ map unpackNum parameters

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

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp op [parameter] = op parameter

primitives :: [(String, [LispVal] -> LispVal)]
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

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

eval :: LispVal -> LispVal
eval (String val) = String val
eval (Character val) = Character val
eval (Number val) = Number val
eval (Bool val) = Bool val
-- evaluation of quoted exp: take off the val
eval (List [Atom "quote", val]) = val

eval (List (Atom func : args)) = apply func $ map eval args
