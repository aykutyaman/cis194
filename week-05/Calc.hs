module Calc where

import ExprT
import Parser (parseExp)

-- Exercise 1
-- eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

-- Exercise 2
-- evalStr "2+2+3*4" == Just 16

evalStr :: String -> Maybe Integer
evalStr = (eval <$>) . parseExp Lit Add Mul

-- Exercise 3
-- (mul (add (lit 2) (lit 3)) (lit 4) :: ExprT)
--   == Mul (Add (Lit 2) (Lit 3)) (Lit 4)
-- lit 3 :: ExprT == Lit 3

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add 
  mul = Mul

reify :: ExprT -> ExprT
reify = id

-- Exercise 4
-- Make instances of Expr for each of the following types:
-- Integer, Bool, MinMax, Mod7

instance Expr Integer where
  lit n = n
  add n m = n + m
  mul n m = n * m

toBool :: Integer -> Bool
toBool n = n > 0

instance Expr Bool where
  lit = toBool
  add = (||)
  mul = (&&)

-- cheated
instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y) = MinMax (min x y)

instance Expr Mod7 where
  lit n = Mod7 (n `mod` 7)
  add (Mod7 n) (Mod7 m) = Mod7 ((n + m) `mod` 7)
  mul (Mod7 n) (Mod7 m) = Mod7 ((n * m) `mod` 7)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7   = Mod7 Integer deriving (Eq, Show)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 2"

-- Try printing out each of these tests in ghci to see if things are working.
testInteger = testExp :: Maybe Integer
testBool    = testExp :: Maybe Bool
testMM      = testExp :: Maybe MinMax
testSat     = testExp :: Maybe Mod7

