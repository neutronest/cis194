module Calc where

import ExprT
import Parser

{-- Exercise 1 --}
eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add exprL exprR) = (eval exprL) + (eval exprR)
eval (Mul exprL exprR) = (eval exprL) * (eval exprR)

{-- Exercise 2 --}
evalStr :: String -> Maybe Integer
evalStr str = let expression = parseExp Lit Add Mul str in
  case expression of
    Just e -> Just (eval e)
    Nothing -> Nothing

{-- Exercise 3 --}
class Expr a where
  lit :: Integer -> a 
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit n = Lit n

  add e1 e2 = Add e1 e2

  mul e1 e2 = Mul e1 e2

instance Expr Integer where
  lit n = n

  add x y = x + y

  mul x y = x * y 


instance Expr Bool where
  lit n | n <= 0 = False
        | otherwise = True

  add x y | x == True = True
          | y == True = True
          | otherwise = False

  mul x y | (x == True) && (y == True) = True
          | otherwise = False

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit n = MinMax n
  add (MinMax x) (MinMax y) = MinMax $ maximum [x,y]
  mul (MinMax x) (MinMax y) = MinMax $ minimum [x,y]

newtype Mon7 = Mon7 Integer deriving (Eq, Show)

instance Expr Mon7 where
  lit n = Mon7 (mod n 7)

  add (Mon7 x) (Mon7 y) = Mon7 $ mod (x+y) 7

  mul (Mon7 x) (Mon7 y) = Mon7 $ mod (x*y) 7
