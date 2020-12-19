{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module JoinList where

import Data.Semigroup
import Sized
import Scrabble
import Buffer
import Editor

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)


instance Buffer (JoinList (Score, Size) String) where
  toString = unlines . jlToList
  fromString           = foldl (\jl str -> jl +++ scoreLine' str) Empty . lines
    where scoreLine' str = Single (scoreString str, 1) str

  line                 = indexJ

  replaceLine n str jl = takeJ n jl +++ fromString str +++ dropJ (n+1) jl

  numLines             = getSize . snd . tag

  value                = getScore . fst . tag

main = runEditor editor (fromString "test" :: (JoinList (Score, Size) String))

sample0 :: JoinList (Product Integer) Char
sample0 = Append (Product 210)
  (Append (Product 30)
   (Single (Product 5) 'y')
   (Append (Product 6)
    (Single (Product 2) 'e')
    (Single (Product 3) 'a')))
  (Single (Product 7) 'h')

-- Exercise 1:
-- Write an append function for JoinList that yields a new JoinList
-- whose monoidal annotation is ervied from those of the two arguments
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) l1 l2 = Append (tag l1 <> tag l2) l1 l2

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

-- Exercise 2:
-- Implement the indexJ function that finds the JoinList element at
-- the specifed index. If the index is out of bounds, the function
-- returns Nothing.

sample1 = Append (Size 4)
  (Append (Size 3)
   (Single (Size 1) 'y')
   (Append (Size 2)
    (Single (Size 1) 'e')
    (Single (Size 1) 'a')))
  (Single (Size 1) 'h')

sample2 = Append (Size 2)
          (Single (Size 1) 'y')
          (Single (Size 1) 'e')

-- 2.1 Implement the function
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i Empty = Nothing
indexJ i (Single m a)
  | i < 0 = Nothing
  | otherwise = Just a
indexJ i (Append m l r)
  | i < 0 || i >= centerSize = Nothing
  | i < leftSize = indexJ i l
  | otherwise  = indexJ (i-leftSize) r
    where
      centerSize = getSize $ size m
      leftSize = getSize $ size $ tag l
-- (indexJ i sample1) == (jblToList sample1 !!? i)


-- 2.2 Implement the function. dropJ drops the first n elements from a JoinList
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ i l@(Single _ _) | i <= 0 = l
dropJ i ll@(Append m l r)
  | i < 0 || i >= centerSize = Empty
  | i >= leftSize = dropJ (i-leftSize) r
  | i > 0 = dropJ i l +++ r
  | otherwise = ll
    where
      centerSize = getSize $ size m
      leftSize = getSize $ size $ tag l
dropJ _ _ = Empty

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ i l@(Single _ _) | i > 0 = l
takeJ i ll@(Append m l r)
  | i < 0 || i >= centerSize = ll
  | i >= leftSize = l +++ takeJ (i-leftSize) r
  | i > 0 = takeJ i l
    where
      centerSize = getSize $ size m
      leftSize = getSize $ size $ tag l
takeJ _ _ = Empty

-- jblToList (dropJ 1 sample1) == drop 1 (jblToList sample1)

scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2
