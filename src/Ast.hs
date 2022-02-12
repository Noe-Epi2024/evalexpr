module Ast where

import Control.Applicative
import Data.Functor.Contravariant (Op (Op))
import Data.List
import Data.Maybe
import Data.Tuple
import Parser
import Text.Printf

data Expr
  = Res Float
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  deriving (Eq, Show)

calc :: Expr -> Float
calc (Res x) = x
calc (Add x y) = calc x + calc y
calc (Sub x y) = calc x - calc y
calc (Mul x y) = calc x * calc y
calc (Div x y) = calc x / calc y
calc (Pow x y) = calc x ** calc y

parseExpr :: Parser Expr
parseExpr = add
  where
    add = case Add <$> mul of
      res -> case res <*> (parseChar '+' *> add) of
        result -> result <|> sub
    sub = case Sub <$> mul of
      res -> case res <*> (parseChar '-' *> add) of
        result -> result <|> mul
    mul = case Mul <$> pow of
      res -> case res <*> (parseChar '*' *> mul) of
        result -> result <|> div
    div = case Div <$> pow of
      res -> case res <*> (parseChar '/' *> mul) of
        result -> result <|> pow
    pow = case Pow <$> factor of
      res -> case res <*> (parseChar '^' *> pow) of
        result -> result <|> factor
    factor = par <|> res
    par = parseChar '(' *> parseExpr <* parseChar ')' <|> res
    res = Res <$> parseFloat

-- Add ::= Mul + Add <|> Mul - Add <|> Mul
-- Mul ::= Pow * Mul <|> Pow / Mul <|> Pow
-- Pow ::= Res ^ Pow <|> Res
-- Res ::= Digit <|> (Add)