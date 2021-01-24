module Fibonacci where

-- Exercise 1:
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2:
fibs2 :: [Integer]
fibs2 = 0:1:zipWith (+) fibs2 (tail fibs2)

fibs2' :: [Integer]
fibs2' = [0, 1] ++ [fibs2' !! (n-1) + fibs2' !! (n-2) | n <- [2..]]

factlist :: [Integer]
factlist = 1:zipWith (*) [1..] factlist

-- Exercise 3:
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x sa) = x:streamToList sa

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

-- Exercise 4:
streamRepeat :: a -> Stream a
streamRepeat x = sx
  where sx = x `Cons` sx

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x sa) = f x `Cons` streamMap f sa

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- https://bit.ly/3mbGyP1
ruler :: Stream Integer
ruler = twistFrom 0 where
  twistFrom n = interleaveStreams (streamRepeat n) (twistFrom (n + 1))

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons m ms) ns =
  Cons m (interleaveStreams ns ms)
