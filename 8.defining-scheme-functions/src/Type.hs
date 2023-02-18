module Type where

import Data.Ratio
import Data.Complex
import Data.IORef
import Text.ParserCombinators.Parsec

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Number
             | String String
             | Character Char
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params :: [String]
                    , restParams :: (Maybe String)
                    , body :: [LispVal]
                    , closure :: Env
                    }

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

-- This declares an `Env` as an `IORef` holding a list that maps `String`s to
-- mutable `LispVal`s.
type Env = IORef [(String, IORef LispVal)]
