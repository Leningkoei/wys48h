module Evaluator where

import Control.Monad.Except
import Data.Foldable

import Environment
import Type
import Error
import Reader

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (IOFunc func) args = func args
apply (Func params restParams body closure) args =
  if length params /= length args && restParams == Nothing
     then throwError $ NumArgs (toInteger $ length params) args
     else return closure >>= bindParams >>= bindRestParams >>= evalBody
  where restArgs = drop (length params) args
        bindParams env = liftIO $ bindVars env $ zip params args
        bindRestParams env = case restParams of
          Nothing -> return env
          Just restParams -> liftIO $ bindVars env [(restParams, List $ restArgs)]
        -- evalBody env = last $ map (eval env) body
        evalBody env = liftM last $ mapM (eval env) body

atom2string :: LispVal -> String
atom2string (Atom atom) = atom
-- TODO: atom2string notAtom = throwError $ TypeMismatch "Atom" notAtom

qqIter :: LispVal -> LispVal -> IOThrowsError LispVal
qqIter (List [Atom "unquotesplicing", x]) acc = return $ List [Atom "concat", x, acc]
qqIter (List (Atom "unquotesplicing" : smjb)) _ = throwError $ NumArgs 1 smjb
qqIter elt acc = do
  -- quoted <- quasiquote elt
  quoted <- return $ List [Atom "quote", elt]
  return $ List [Atom "cons", quoted, acc]
quasiquote :: LispVal -> IOThrowsError LispVal
quasiquote (List [Atom "unquote", atomX]) = return atomX
quasiquote (List (Atom "unquote" : listX)) = throwError $ NumArgs 1 listX
quasiquote (List list) = foldrM qqIter (List [Atom "quote", List []]) list
quasiquote atom = return $ atom

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env (Atom var) = getVar env var
eval _ (String string) = return $ String string
eval _ (Character character) = return $ Character character
eval _ (Number number) = return $ Number number
eval _ (Bool boolean) = return $ Bool boolean
eval _ (List [Atom "quote", form]) = return form
-- eval env (List [Atom "quasiquote", form]) = func env form
--   where func :: Env -> LispVal -> IOThrowsError LispVal
--         func env (List [Atom "unquote", form]) = eval env form
--         func env (List forms) = mapM (func' env) forms >>= return . List
--           where func' :: Env -> LispVal -> IOThrowsError LispVal
--                 func' env (List [Atom "unquote", form]) = eval env form
--                 func' _ form = return form
eval env (List [Atom "quasiquote", form]) = quasiquote form >>= eval env
eval env (List [Atom "if", checkForm, thenForm, elseForm]) = do
  checked <- eval env checkForm
  case checked of
    Bool True -> eval env thenForm
    Bool False -> eval env elseForm
eval env (List [Atom "define!", Atom var, valForm]) =
  eval env valForm >>= defineVar env var
eval env (List [Atom "set!", Atom var, valForm]) =
  eval env valForm >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
  return $ Func (map atom2string params) Nothing body env
eval env (List (Atom "lambda" : DottedList params restParams : body)) =
  return $ Func (map atom2string params) (Just $ atom2string restParams) body env
eval env (List [Atom "load", String filename]) =
  load filename >>= liftM last . mapM (eval env)
eval env (List (func : args)) = do
  func' <- eval env func
  args' <- mapM (eval env) args
  apply func' args'
eval _ badForm = throwError . UnboundVar "Unbound value" $ show badForm
