module Lecture where

hailstone :: Integer -> Integer
hailstone n
  | even n    = n `div` 2
  | otherwise = 3 * n + 1

hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

intListLength :: [Integer] -> Integer
intListLength [] = 0
intListLength (_:xs) = 1 + intListLength xs

hailstoneLen :: Integer -> Integer
hailstoneLen n = intListLength (hailstoneSeq n ) - 1


