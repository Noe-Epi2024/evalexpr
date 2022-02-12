module Lib where

import Data.Maybe
import Data.Tuple
import Data.List
import Text.Printf
-- Parser function
import Parser
import Error
import Ast

runParser' :: Parser a -> String -> Maybe a
runParser' (Parser a) str = case a str of
    Just (res, "") -> Just res
    _ -> Nothing

evalExpr :: String -> Maybe Float
evalExpr str = fmap calc $ runParser' parseExpr $ concat $ words str

-- Calc part

-- evalExpr :: String -> IO ()
-- evalExpr str = case parseFloat_ str of
--     Just (v1,next) -> case parseAnyChar_ "+-*/^" next of
--         Just (sign,next) -> case parseFloat_ next of
--             Just (v2,end) -> case parseCalc sign v1 v2 of
--                 Left a -> print a >> errorExit
--                 Right b -> printf "%.2f\n" b
--             Nothing -> errorExit
--         Nothing -> errorExit
--     Nothing -> errorExit

-- parseCalc :: Char -> Float -> Float -> Either String Float
-- parseCalc '+' v1 v2 = Right (calcAdd v1 v2)
-- parseCalc '-' v1 v2 = Right (calcSub v1 v2)
-- parseCalc '*' v1 v2 = Right (calcMul v1 v2)
-- parseCalc '/' v1 v2 = Right (calcDiv v1 v2)
-- parseCalc '^' v1 v2 = Right (calcPow v1 v2)
-- parseCalc _ _ _ = Left "Error in parseCalc"

-- calcAdd :: Float -> Float -> Float
-- calcAdd a b = a + b

-- calcSub :: Float -> Float -> Float
-- calcSub a b = a - b

-- calcMul :: Float -> Float -> Float
-- calcMul a b = a * b

-- calcDiv :: Float -> Float -> Float
-- calcDiv a b = a / b

-- calcPow :: Float -> Float -> Float
-- calcPow a b = a ** b

-- intToFloat :: Int -> Float
-- intToFloat x = fromIntegral x :: Float