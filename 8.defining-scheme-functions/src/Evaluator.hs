module Evaluator where

import Control.Monad.Except

import Environment
import Type
import Error

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params restParams body closure) args =
  if length params /= length args && restParams == Nothing
     then throwError $ NumArgs (toInteger $ length params) args
     else return closure >>= bindParams >>= bindRestParams >>= evalBody
  where restArgs = drop (length params) args
        bindParams env = liftIO $ bindVars env $ zip params args
        bindRestParams env = case restParams of
          Nothing -> return env
          Just restParams -> liftIO $ bindVars env [(restParams, List $ restArgs)]
        evalBody env = last $ map (eval env) body

atom2string :: LispVal -> String
atom2string (Atom atom) = atom
-- TODO: atom2string notAtom = throwError $ TypeMismatch "Atom" notAtom

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env (Atom var) = getVar env var
eval _ (String string) = return $ String string
eval _ (Character character) = return $ Character character
eval _ (Number number) = return $ Number number
eval _ (Bool boolean) = return $ Bool boolean
eval _ (List [Atom "quote", form]) = return $ form
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
eval env (List (func : args)) = do
  func' <- eval env func
  args' <- mapM (eval env) args
  apply func' args'
eval _ badForm = throwError . UnboundVar "Unbound value" $ show badForm
