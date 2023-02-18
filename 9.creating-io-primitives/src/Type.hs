module Type where

import System.IO
import Data.Ratio
import Data.Complex
import Data.IORef
import Control.Monad.Except
import Text.ParserCombinators.Parsec

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Number
             | String String
             | Character Char
             | Bool Bool
             | Port Handle
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Func [String] (Maybe String) [LispVal] Env
             -- | Func { params :: [String]
             --        , restParams :: (Maybe String)
             --        , body :: [LispVal]
             --        , closure :: Env
             --        }

data Number = Integer Integer
            | Float Double
            | Ratio Rational
            | Complex (Complex Double)

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

type ThrowsError = Either LispError
type IOThrowsError = ExceptT LispError IO

-- This declares an `Env` as an `IORef` holding a list that maps `String`s to
-- mutable `LispVal`s.
type Env = IORef [(String, IORef LispVal)]
