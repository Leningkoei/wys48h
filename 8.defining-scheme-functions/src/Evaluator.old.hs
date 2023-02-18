-- {-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module Evaluator.Old where

import Control.Monad.Except

import Type
import Error
import Environment

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = do
  unpacked1 <- unpacker arg1
  unpacked2 <- unpacker arg2
  return $ unpacked1 == unpacked2
  `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
    [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqEquals <- eq [arg1, arg2]
  let Bool eqEquals' = eqEquals in
    return $ Bool $ (primitiveEquals || eqEquals')

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number (Integer n)) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum
unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool bool) = return bool
unpackBool notBool = throwError $ TypeMismatch "bool" notBool
unpackStr :: LispVal -> ThrowsError String
unpackStr (String str) = return str
unpackStr notStr = throwError $ TypeMismatch "string" notStr

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [parameter] = throwError $ NumArgs 2 [parameter]
numericBinop op parameters = mapM unpackNum parameters >>=
  return . Number . Integer . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) ->
             (a -> a -> Bool) -> [LispVal] ->
             ThrowsError LispVal
boolBinop _ _ [] = throwError $ NumArgs 2 []
boolBinop _ _ [parameter] = throwError $ NumArgs 2 [parameter]
boolBinop _ _ (x:y:z:rest) = throwError $ NumArgs 2 $ x:y:z:rest
boolBinop unpacker op parameters =
  mapM unpacker parameters >>=
  \ [x, y] -> return . Bool $ op x y

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop unpackNum
boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool
strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr

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

car :: [LispVal] -> ThrowsError LispVal
car [List (x:_)] = return x
car [DottedList (x:_) _] = return x
car [notPair] = throwError $ TypeMismatch "pair" notPair
car parameters = throwError $ NumArgs 1 parameters

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_:xs)] = return $ List xs
cdr [DottedList _ x] = return x
cdr [notPair] = throwError $ TypeMismatch "pair" notPair
cdr parameters = throwError $ NumArgs 1 parameters

cons :: [LispVal] -> ThrowsError LispVal
cons [car, List cdr] = return . List $ car : cdr
cons [car', DottedList car cdr] = return $ DottedList (car':car) cdr
cons [car, cdr] = return $ DottedList [car] cdr
cons parameters = throwError $ NumArgs 2 parameters

eq :: [LispVal] -> ThrowsError LispVal
eq [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eq [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eq [(Number (Integer arg1)), (Number (Integer arg2))] =
  return $ Bool $ arg1 == arg2
eq [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eq [(DottedList xs x), (DottedList ys y)] =
  eq [List $ x : xs, List $ y : ys]
eq [(List xs), (List ys)] =
  return $ Bool $ length xs == length ys &&
  (all eqPair $ zip xs ys) where
    eqPair :: (LispVal, LispVal) -> Bool
    eqPair (a, b) = case eq [a, b] of
      Right (Bool result) -> result
eq [_, _] = return $ Bool False
eq mismatchParameters = throwError $ NumArgs 2 mismatchParameters

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
             , ("==", numBoolBinop (==))
             , ("/=", numBoolBinop (/=))
             , (">", numBoolBinop (>))
             , ("<", numBoolBinop (<))
             , (">=", numBoolBinop (>=))
             , ("<=", numBoolBinop (<=))
             , ("string==", strBoolBinop (==))
             , ("string/=", strBoolBinop (/=))
             , ("string>", strBoolBinop (>))
             , ("string<", strBoolBinop (<))
             , ("string>=", strBoolBinop (>=))
             , ("string<=", strBoolBinop (<=))
             , ("&&", boolBoolBinop (&&))
             , ("||", boolBoolBinop (||))
             , ("car", car)
             , ("cdr", cdr)
             , ("cons", cons)
             , ("=", eq)
             , ("=!", equal)
             ]

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe
  (throwError $ NotFunction "Unrecognized primitive function args" func)
  ($ args) $ lookup func primitives

eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ (String string) = return $ String string
eval _ (Character character) = return $ Character character
eval _ (Number number) = return $ Number number
eval _ (Bool boolean) = return $ Bool boolean
eval _ (List [Atom "quote", value]) = return $ value
eval env (List [Atom "if", checkForm, thenForm, elseForm]) = do
  result <- eval env checkForm
  case result of
    Bool True -> eval env thenForm
    Bool False -> eval env elseForm
eval env (Atom var) = getVar env var
eval env (List [Atom "define", Atom var, valForm]) =
  eval env valForm >>= defineVar env var
eval env (List [Atom "set!", Atom var, valForm]) =
  eval env valForm >>= setVar env var
eval env (List (Atom func : args)) = mapM (eval env) args >>=
  liftThrows . apply func
eval env badForm = throwError . UnboundVar "Unbound value" $ show badForm
