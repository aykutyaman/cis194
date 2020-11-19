module Golf where

import qualified Data.Map as Map

-- For each task, you should submit a function with the required name and
-- type signature which accomplishes the given task and is as short as possible

-- Ex 1: Hopscotch
-- skips "ABCD" == ["ABCD", "CD", "C", "D"]
-- skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]

filterZ :: Integer -> (a, Integer) -> Bool
filterZ z (_, i) = mod (i+1) (z+1) == 0

go :: [(a, Integer)] -> [a]
go xs = map fst $ filter (filterZ . (snd . head) $ xs) xs

skips :: [a] -> [[a]]
skips ls = loop (zip ls [0..])
  where
    loop [] = []
    loop l@(_:xs) = go l : loop xs

-- Ex 2: Local maxima
-- localMaxima [2,9,5,6,1] == [9,6]
-- localMaxima [2,3,4,1,6] == [4]
-- localMaxima [1,2,3,4,5] == []

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:r)
  | y > z && y > x = y : localMaxima (y:z:r)
  | otherwise = localMaxima (y:z:r)
localMaxima _ = []

-- Ex 3: Histogram
-- putStr (histogram [3,5])

insertNumber :: Integer -> Map.Map Integer Integer -> Map.Map Integer Integer
insertNumber x = Map.insertWith (+) x 1

look :: Map.Map Integer Integer -> Integer -> Integer -> Char
look hm level x =
  case Map.lookup x hm of
    Nothing -> ' '
    Just n -> if n >= level then '*' else ' '

toLine :: Map.Map Integer Integer -> Integer -> String
toLine hm level = map (look hm level) $ reverse [0..9]

toLines :: Map.Map Integer Integer -> [Integer] -> String
toLines dict = reverse . unlines . map (toLine dict)

histogram :: [Integer] -> String
histogram xs = toLines dict [1..maxLevel] ++
  "\n==========\n0123456789\n"
  where
    dict = foldr insertNumber Map.empty xs
    maxValue = maximum $ Map.elems dict
    maxLevel = snd $ head $ Map.toList $ Map.filter (== maxValue) dict
