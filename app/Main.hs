module Main where

import System.Environment
import Data.List
import Data.Char
import Data.Typeable
import Control.Exception
import System.Exit
import Text.Read
import Text.Printf
import Numeric

import Lib
import Error
import Parser

main :: IO ()
main = do
    args <- getArgs
    case removeChar ' ' $ unlines args of
        [] -> printf "Error: wrong usage, try ./funEvalExpr “3 + 5.34”\n" >> errorExit
        string -> case evalExpr string of
            Just str -> printf "%.2f\n" str
            Nothing -> printf "Error: something went wrong during parsing\n" >> errorExit