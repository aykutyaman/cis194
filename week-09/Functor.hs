module Functor where

import Test.QuickCheck

replaceWithP :: b -> Char
replaceWithP = const 'b'

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]

-- Just making the argument more specific
replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

-- But we can assert a more specific type for liftedReplace!
liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

twiceLifted :: (Functor f1, Functor f) => f (f1 a) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP

-- making more specific
twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted
-- f ~ []
-- f1 ~ Maybe

thriceLifted :: (Functor f, Functor f1, Functor f2) => f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP

-- more specific or "concrete"
thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted
-- f ~ []
-- f1 ~ Maybe
-- f2 ~ []

main :: IO ()
main = do
  putStr "replaceWithP' lms:\t"
  print (replaceWithP' lms)

  putStr "liftedReplace lms:\t"
  print (liftedReplace lms)

  putStr "liftedReplace' lms:\t"
  print (liftedReplace' lms)

  putStr "twiceLifted lms:\t"
  print (twiceLifted lms)
  
  putStr "twiceLifted' lms:\t"
  print (twiceLifted' lms)

  putStr "thriceLifted lms:\t"
  print (thriceLifted lms)

  putStr "thriceLifted' lms:\t"
  print (thriceLifted' lms)

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = fmap g (fmap f x) == fmap (g . f) x

functorCompose' :: (Eq (f c), Functor f) =>
                   f a
                -> Fun a b
                -> Fun b c
                -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool
  
-- Exercises: Instances of Func
-- Implement Functor instances for the following datatypes.
-- 1.
newtype Identity a = Identity a
  deriving (Show, Eq)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

identity :: (Functor f, Eq (f a)) => f a -> Bool
identity f = fmap id f == f

genId :: Arbitrary a => Gen (Identity a)
genId = do
  x <- arbitrary
  return $ Identity x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = genId

-- 2.
data Pair a = Pair a a
  deriving (Show, Eq)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

genPair :: Arbitrary a => Gen (Pair a)
genPair = do
  x <- arbitrary
  y <- arbitrary
  return $ Pair x y

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = genPair

-- 3.
data Two a b = Two a b
  deriving (Show, Eq)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

genTwo :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
genTwo = do
  x <- arbitrary
  y <- arbitrary
  return $ Two x y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = genTwo

-- 4.
data Three a b c = Three a b c
  deriving (Show, Eq)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

genThree :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c) 
genThree = do
  x <- arbitrary
  y <- arbitrary
  z <- arbitrary
  return $ Three x y z

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = genThree

-- 5.
data Four a b c d = Four a b c d
  deriving (Show, Eq)

instance Functor (Four a b c) where
  fmap f (Four x y z d) = Four x y z (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
  Arbitrary (Four a b c d) where
  arbitrary = genFour

genFour :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d) 
genFour = do
  x <- arbitrary
  y <- arbitrary
  z <- arbitrary
  q <- arbitrary
  return $ Four x y z q


test :: IO ()
test = do
  quickCheck $ \x -> functorIdentity (x :: [Int])
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: [Int])
  quickCheck (functorCompose' :: IntFC)

  -- Identity
  quickCheck $ \x -> functorIdentity (x :: Identity Int)
  quickCheck $ \x -> functorIdentity (x :: Identity String)
  quickCheck (functorCompose' :: Identity Int -> IntToInt -> IntToInt -> Bool)

  -- Pair
  quickCheck $ \x -> functorIdentity (x :: Pair Int)
  quickCheck (functorCompose' :: Pair Int -> IntToInt -> IntToInt -> Bool)

  -- Two
  quickCheck $ \x -> functorIdentity (x :: Two Int Int)
  quickCheck (functorCompose' :: Two Int Int -> IntToInt -> IntToInt -> Bool)

  -- Three
  quickCheck $ \x -> functorIdentity (x :: Three Int Int Int)
  quickCheck (functorCompose' :: Three Int Int Int -> IntToInt -> IntToInt -> Bool)

  -- Four
  quickCheck $ \x -> functorIdentity (x :: Four Int Int Int Int)
  quickCheck (
    functorCompose' :: Four Int Int Int Int -> IntToInt -> IntToInt -> Bool)

  putStrLn "DONE"
