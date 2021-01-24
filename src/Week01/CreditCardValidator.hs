module CreditCardValidator where

-- Exercise 1
-- toDigits 1234 == [1,2,3,4]
toDigits :: Integer -> [Integer]
toDigits = go [] . abs
  where
    go acc 0 = acc
    go acc x = go (snd b : acc) (fst b) where b = x `divMod` 10

-- Exercise 2
-- doubleEveryOther [1,2,3] == [1,4,3]
-- doubleEveryOther [1,2,3,4] == [2,2,6,4]
-- Double every other number starting from the right
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther l@(x:xs)
  | odd (length l) = x : doubleEveryOther xs
  | otherwise = x * 2 : doubleEveryOther xs

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


