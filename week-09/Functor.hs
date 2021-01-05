{-# LANGUAGE FlexibleInstances #-}
module Functor where

import Test.QuickCheck
import GHC.Arr

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

-- Exercise: Possibly
-- Write a Functor instance for a datatype identical to Maybe
data Possibly a = 
  LoLNope
  | Yeppers a
  deriving (Show, Eq)

instance Functor Possibly where
  fmap _ LoLNope = LoLNope
  fmap f (Yeppers x) = Yeppers $ f x

data Sum a b =
  First a
  | Second b
  deriving (Show, Eq)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second $ f b

data Constant a b =
  Constant { getConstant :: a }
  deriving (Show, Eq)

-- 
instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

-- 16.13

data Wrap f a =
  Wrap (f a)
  deriving (Show, Eq)

instance Functor f => Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap (fmap f fa)

getInt :: IO Int
getInt = fmap read getLine

-- 16.17
-- Determine if a valid Functor can be written for the datatype provided

-- 1. No
-- data Bool = False | True
-- because it's kind it's not * -> *

-- 2. Yes
data BoolAndSomethingElse a =
  False' a | True' a
  deriving (Show, Eq)

-- Yes, we can't have a value to map over
instance Functor BoolAndSomethingElse where
  fmap f (False' x) = False' (f x)
  fmap f (True' x) = True' (f x)

-- 3. Yes
data BoolAndMaybeSomethingElse a =
  Falsish | Truish a
  deriving Show

instance Functor BoolAndMaybeSomethingElse where
  fmap _ Falsish = Falsish
  fmap f (Truish x) = Truish (f x)

-- 4. No, Mu has the wrong kind
newtype Mu f = InF { outF :: f (Mu f) } 

-- 5. Again, D has the wrong kind *
data D = D (Array Word Word) Int Int

-- Rearrange the arguments to the type constructor of the datatype so
-- the Functor instance works
-- 1.
data Sum' a b =
  First' a
  | Second' b
  deriving Show

instance Functor (Sum' e) where
  fmap _ (First' x) = First' x
  fmap f (Second' x) = Second' $ f x

-- 2.
data Company a b c =
  DeepBlue a b
  | Something c

instance Functor (Company a b) where
  fmap _ (DeepBlue x y) = DeepBlue x y
  fmap f (Something x) = Something $ f x

-- 3.
data More b a =
  L a b a
  | R b a b
  deriving Show

instance Functor (More a) where
  fmap f (L x y x') = L (f x) y (f x')
  fmap f (R x y x') = R x (f y) x'

-- Write functor instances for the following datatypes
-- 1.
data Quant a b =
  Finance
  | Desk a
  | Bloor b
  deriving Show

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk x) = Desk x
  fmap f (Bloor x) = Bloor $ f x


-- 2.
data K a b =
  K a
  deriving Show

instance Functor (K a) where
  fmap _ (K x) = K x

-- 3.
newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

newtype K' a b =
  K' a
  deriving Show

instance Functor (Flip K' a) where
  fmap f (Flip (K' x)) = Flip $ K' (f x)

-- 4.
data EvilGoateeConst a b =
  GoatyConst b
  deriving Show

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst x) = GoatyConst $ f x

-- 5.
data LiftItOut f a =
  LiftItOut (f a)
  deriving Show

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut $ fmap f fa

-- 6.
data Parappa f g a =
  DaWrappa (f a) (g a)
  deriving Show

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

-- 7.
data IgnoreOne f g a b =
  IgnoreSomething (f a) (g b)
  deriving Show

instance (Functor f, Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoreSomething fa gb) = IgnoreSomething fa (fmap f gb)

-- 8.
data Notorious g o a t =
  Notorious (g o) (g a) (g t)
  deriving Show

instance (Functor g) => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

-- 9.
data List a =
  Nil
  | Cons a (List a)
  deriving Show

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- 10.
data GoatLord a =
  NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
  deriving Show

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat x) = OneGoat $ f x
  fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)

-- 11.
data TalkToMe a =
  Halt
  | Print String a
  | Read (String -> a)
  -- deriving Show

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read g) = Read (f . g)
