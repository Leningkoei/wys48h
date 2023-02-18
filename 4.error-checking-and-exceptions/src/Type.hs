module Type where

import Data.Ratio
import Data.Complex

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Number
             | String String
             | Character Char
             | Bool Bool

data Number = Integer Integer
            | Float Double
            | Ratio Rational
            | Complex (Complex Double)
