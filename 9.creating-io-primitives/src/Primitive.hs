{-# LANGUAGE ExistentialQuantification #-}

module Primitive where

import Control.Monad.Except
import System.IO

import Type
import Error
import Environment
import Reader
import Evaluator


primitiveBindedEnv :: IO Env
primitiveBindedEnv = do
  nullEnv' <- nullEnv
  bindVars nullEnv' $ map makePrimitiveFunc primitives ++ map makeIOFunc ioPrimitives
    -- ++ [("nil", List [])]
  where makePrimitiveFunc (name, func) = (name, PrimitiveFunc func)
        makeIOFunc (name, func) = (name, IOFunc func)


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

concat :: [LispVal] -> ThrowsError LispVal
concat [List a, List b] = return . List $ a ++ b
concat parameters = throwError $ NumArgs 2 parameters

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
             , ("concat", Primitive.concat)
             , ("=", eq)
             , ("=!", equal)
             ]


-- IO primitives

lispApply :: [LispVal] -> IOThrowsError LispVal
lispApply [func, List args] = apply func args
lispApply (func : args) = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port . liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort notaPort = throwError $ TypeMismatch "a Port" $ List notaPort

lispRead :: [LispVal] -> IOThrowsError LispVal
lispRead [] = lispRead [Port stdin]
lispRead [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

lispWrite :: [LispVal] -> IOThrowsError LispVal
lispWrite [obj] = lispWrite [obj, Port stdout]
lispWrite [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String . liftIO $ readFile filename

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [ ("apply", lispApply)
               , ("open-input-file", makePort ReadMode)
               , ("open-output-file", makePort WriteMode)
               , ("close-input-port", closePort)
               , ("close-output-port", closePort)
               , ("read", lispRead)
               , ("write", lispWrite)
               , ("read-contents", readContents)
               , ("read-all", readAll)
               ]
