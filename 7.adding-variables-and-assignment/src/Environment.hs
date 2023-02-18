module Environment where

import Data.IORef
import Control.Monad.Except

import Type
import Error

-- This declares an `Env` as an `IORef` holding a list that maps `String`s to
-- mutable `LispVal`s.
type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ExceptT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError LispVal -> IO String
runIOThrows action = runExceptT action >>= return . handleError

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>=
  return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  value <- return $ lookup var env
  value' <- maybe
    (throwError $ UnboundVar "Getting an unbound variable" var)
    (liftIO . readIORef) value
  return $ value'

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var newValue = do
  env <- liftIO $ readIORef envRef
  value <- return $ lookup var env
  -- maybe (throwError $ UnboundVar "Setting an unbound variable" var)
  --       (liftIO . (flip writeIORef newValue)) value
  value' <- maybe
    (throwError $ UnboundVar "Setting an unbound variable" var)
    return value
  liftIO $ writeIORef value' newValue
  return newValue

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>=
  extendEnv bindings >>= newIORef where
    mapM :: ((String, LispVal) -> IO (String, IORef LispVal))
         -> [(String, LispVal)]
         -> IO [(String, IORef LispVal)]
    mapM = mapM
    extendEnv :: [(String, LispVal)]
              -> [(String, IORef LispVal)]
              -> IO [(String, IORef LispVal)]
    extendEnv bindings env = liftM (++ env) $ mapM addBinding bindings
    -- extendEnv bindings env = mapM addBinding bindings >>= return . (++env)
    addBinding :: (String, LispVal) -> IO (String, IORef LispVal)
    addBinding (var, value) = do
      ref <- newIORef value
      return (var, ref)
