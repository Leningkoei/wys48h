module Printer where

import Data.Ratio
import Data.Complex

import Type

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Character character) = "#\\" ++ character : ""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "("
                              ++ unwordsList head
                              ++ " . "
                              ++ showVal tail
                              ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func { params = params
              , restParams = restParams
              , body = body
              , closure = closure
              }) = "(lambda ("
                ++ unwords params
                ++ (case restParams of
                      Nothing -> ""
                      Just restParams -> " . " ++ restParams)
                ++ ") ... )"

instance Show Number where show = showNumber
showNumber (Integer number) = show number
showNumber (Float number) = show number
showNumber (Ratio number) = (show . numerator) number ++ "/"
                         ++ (show . denominator) number
showNumber (Complex number) = (show . realPart) number ++ "+"
