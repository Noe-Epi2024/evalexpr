import Test.HUnit

import Lib
import Parser
import Error
import Ast

calcTesting :: Test
calcTesting = test [
    assertEqual "Basic Test for evalExpr Addition" (Just 8.34) (evalExpr "3 + 5.34"),
    assertEqual "Basic Test for evalExpr Substraction" (Just (-2.3400002)) (evalExpr "3 - 5.34"),
    assertEqual "Basic Test for evalExpr Multiplication" (Just 16.02) (evalExpr "3 * 5.34"),
    assertEqual "Basic Test for evalExpr Division" (Just 0.56179774) (evalExpr "3 / 5.34"),
    assertEqual "Basic Test for evalExpr Power" (Just 353.04297) (evalExpr "3 ^ 5.34"),
    assertEqual "Basic Test for evalExpr Error" Nothing (evalExpr "2 + 2 ^ a"),

    assertEqual "Intermediate Test for evalExpr Addition" (Just 22) (evalExpr "2 * 5 + 12"),
    assertEqual "Intermediate Test for evalExpr Substraction" (Just (-2.0)) (evalExpr "2 * 5 - 12"),
    assertEqual "Intermediate Test for evalExpr Multiplication" (Just 100) (evalExpr "(2 * 5) * 10"),
    assertEqual "Intermediate Test for evalExpr Division" (Just 0.4) (evalExpr "(20 / 5) / 10"),
    assertEqual "Intermediate Test for evalExpr Power" (Just 160000.0) (evalExpr "20 ^ 5 / 20"),
    assertEqual "Intermediate Test for evalExpr Error" Nothing (evalExpr "()"),


    assertEqual "Hard Test One for calc" (Just (-5.345)) (evalExpr "((0.345+ 5)*(-2-1)) / 3"),
    assertEqual "Hard Test Two for calc" (Just 80) (evalExpr "(12 ^ 2) - 1 * (8 ^ 2)"),
    assertEqual "Hard Test Three for calc" (Just 11.1222) (evalExpr "(999 / 3 + 333 ^ 2) / 10 ^ 4"),
    assertEqual "Hard Test Four for calc" (Just 2.0) (evalExpr "(1 * 1) + 1 ^ (1 / 1)"),
    assertEqual "Hard Test Five for calc" (Just 246.0) (evalExpr "((5 + 5)*(-2-1)) / 3 + 2 ^ 8")]

main :: IO Counts
main = runTestTT calcTesting