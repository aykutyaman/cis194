module CreditCardValidator where

import Data.Char (digitToInt)

-- Exercise 1
-- toDigits 1234 == [1,2,3,4]
toDigits :: Integer -> [Integer]
toDigits x
  | x <= 0 = []
  | otherwise = map (toInteger . digitToInt) . show $ x

-- Exercise 2
-- doubleEveryOther [1,2,3] == [1,4,3]
-- doubleEveryOther [1,2,3,4] == [2,2,6,4]
-- Double every other number starting from the right
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . (go 0 . reverse)
  where
    go i (x:xs)
      | null xs = [dbl i x]
      | otherwise = dbl i x : go (i + 1) xs

dbl :: Integer -> Integer -> Integer
dbl x y
  | odd x = y * 2
  | otherwise = y
-- It should be a better way to do this.

-- https://bit.ly/3eJ3va9
doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' = reverse . zipWith (*) oneTwo . reverse
  where oneTwo = 1 : 2 : oneTwo

-- Exercise 3
-- sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22
sumDigits :: [Integer] -> Integer
sumDigits = sum . foldr spread []

spread :: Integer -> [Integer] -> [Integer]
spread x xs
  | x >= 10 = toDigits x ++ xs
  | otherwise = x : xs

-- Exercise 4
-- validate 4012888888881881 = True
-- validate 4012888888881882 = False
validate :: Integer -> Bool
validate x = mod ((sumDigits . doubleEveryOther . toDigits) x) 10 == 0


