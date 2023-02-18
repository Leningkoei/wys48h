module Error where

import Control.Monad.Except
import Text.ParserCombinators.Parsec

import Type
import Printer

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where show = showError
showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ func
showError (NumArgs expected found) =
  "Expected " ++ show expected ++ " and more args; found values " ++ unwordsList found
showError (TypeMismatch expected found) =
  "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

type ThrowsError = Either LispError

trapError :: ThrowsError [Char] -> ThrowsError [Char]
trapError action = catchError action (return . show)
-- error2value :: LispError -> ThrowsError LispVal
-- error2value = return . String . show
-- trapError :: ThrowsError LispVal -> ThrowsError LispVal
-- trapError action = catchError action error2value

-- extract value: Throw Error if it exists, or return value which is inside ThrowsError.
extractValue :: ThrowsError a -> a
extractValue (Right val) = val
