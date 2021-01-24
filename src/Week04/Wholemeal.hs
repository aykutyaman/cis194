module Wholemeal where

-- Ex 1: Reimplement each of the following functions in a more idiomatic way

-- 1. Wholemeal Programming

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

-- fun1' [3,4,5,6] == 8
fun1' :: [Integer] -> Integer
fun1' = foldr ((*) . subtract 2) 1 . filter even

-- 2.
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

iterator :: Integer -> Integer
iterator x
  | even x = x `div` 2
  | otherwise = 3 * x + 1

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (> 1) . iterate iterator

-- 2. Folding with trees
-- I had to cheat to solve this problem.
-- My foldr usage intuition was correct, but not the balancing part. Maybe because
-- I tried to solve it placing the nodes in the exact order which I found pretty tough.
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Ord, Eq, Show)

treeLevel :: Tree a -> Integer
treeLevel Leaf = 0
treeLevel (Node n _ _ _) = n

insert :: (Ord a) => a -> Tree a -> Tree a
insert y Leaf = Node 0 Leaf y Leaf
insert y (Node _ left n right)
  | left > right = Node (treeLevel newRight + 1) left n newRight
  | otherwise = Node (treeLevel newLeft + 1) newLeft n right
  where
    newRight = insert y right
    newLeft = insert y left

foldTree :: (Ord a) => [a] -> Tree a
foldTree = foldr insert Leaf

-- Exercise 3:

-- 1. Implement xor
-- xor [False, True, False] == True
-- xor [False, True, False, False, True] == False

xor :: [Bool] -> Bool
xor = odd . foldr ((+) . (\b -> if b then 1 else 0)) 0

-- 2. Implement map as a fold
map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

-- 3. Optional Implement foldl using foldr
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl'= foldr . flip
-- ?? - unable to solve
-- myFoldl :: (a -> b -> a) -> a -> [b] -> a
-- myFoldl f base xs = foldr (\b g x -> g (f x b)) id xs base
-- https://wiki.haskell.org/Foldl_as_foldr_alternative
-- https://stackoverflow.com/questions/6172004/writing-foldl-using-foldr
