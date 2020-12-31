module Party where

import System.Environment

import Data.Tree

-- Employee names are represented by Strings.
type Name = String

-- The amount of fun an employee would have at the party, represented
-- by an Integer
type Fun  = Integer

-- An Employee consists of a name and a fun score.
data Employee = Emp { empName :: Name, empFun :: Fun }
  deriving (Show, Read, Eq)

test :: Tree Employee
test = Node (Emp "aykut" 4) [ Node (Emp "mehmet" 9) []]

-- A small company hierarchy to use for testing purposes.
testCompany :: Tree Employee
testCompany
  = Node (Emp "Stan" 9)
    [ Node (Emp "Bob" 3)
      [ Node (Emp "Joe" 5)
        [ Node (Emp "John" 1) []
        , Node (Emp "Sue" 5) []
        ]
      , Node (Emp "Fred" 3) []
      ]
    , Node (Emp "Sarah" 17)
      [ Node (Emp "Sam" 4) []
      ]
    ]

testCompany2 :: Tree Employee
testCompany2
  = Node (Emp "Stan" 9)
    [ Node (Emp "Bob" 3) -- (8, 8)
      [ Node (Emp "Joe" 5) -- (5, 6)
        [ Node (Emp "John" 1) [] -- (1, 0)
        , Node (Emp "Sue" 5) [] -- (5, 0)
        ]
      , Node (Emp "Fred" 3) [] -- (3, 0)
      ]
    , Node (Emp "Sarah" 17) -- (17, 4)
      [ Node (Emp "Sam" 4) [] -- (4, 0)
      ]
    ]

-- A type to store a list of guests and their total fun score.
data GuestList = GL [Employee] Fun
  deriving (Show, Eq)

instance Ord GuestList where
  compare (GL _ f1) (GL _ f2) = compare f1 f2

glMap :: (Employee -> a) -> GuestList -> [a]
glMap f (GL xs _) = fmap f xs

testGuestList0 :: GuestList
testGuestList0 = GL [Emp "Stan" 9, Emp "Bob" 3] 12

testGuestList1 :: GuestList
testGuestList1 = GL [Emp "John" 1, Emp "Sue" 5] 6

-- Exercise 1
-- Tools for working with GuestLists
-- 1. Add an Employee to the GuestList (updating the cached Fun score appropriatly)
glCons :: Employee -> GuestList -> GuestList
glCons emp (GL es p) = GL (emp:es) (p + empFun emp)

-- 2. A Monoid instance for GuestList
-- testGuestList0 <> testGuestList1
instance Semigroup GuestList where
  (GL l1 fun1) <> (GL l2 fun2) = GL (l1 ++ l2) (fun1 + fun2)

instance Monoid GuestList where
  mempty = GL [] 0

-- 3. Returns the guest list with higher fun
moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1 gl2 | gl1 > gl2 = gl1
                | otherwise = gl2

-- Exercise 2
-- Define a treeFold function for Data.Tree
-- data Tree' a = Node' {
--   rootLabel :: a,
--   subForest :: [Tree' a]
--                    }
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node root xs) = f root (treeFold f <$> xs)

testCompany' :: Tree Employee
testCompany'
  = Node (Emp "Stan" 9)
    [ Node (Emp "Bob" 3)
      [ Node (Emp "Joe" 5)
        [ Node (Emp "John" 1) []
        , Node (Emp "Sue" 5) []
        ]
      , Node (Emp "Fred" 3) []
      ]
    , Node (Emp "Sarah" 17)
      [ Node (Emp "Sam" 4) []
      ]
    ]

ex1 = treeFold (\x xs -> x : mconcat xs) test
ex2 = treeFold (\x xs -> glCons x $ mconcat xs) test
ex3 = treeFold (\x xs -> glCons x $ mconcat xs) testCompany'

-- The algorithm
-- Exercise 3
-- nextLevel should compute the overall best guest list that includes boss,
-- and overall best guest list that doesn't include boss.
-- the first in the pair is the best possible guest list _with_ the boss
-- the second is the best possible guest list _without_ the boss
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss gls = (withBoss, withoutBoss)
  where withoutBoss = mconcat (map (uncurry moreFun) gls)
        withBoss    = glCons boss (mconcat (map snd gls))

-- Exercise 4
-- maxFun takes a company hierarchy as input and outputs a fun-maximizing guest list
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

-- Exercise 5
-- implement main so that it reads your company's hierarchy from the file
-- company.txt, and then prints out a formatted guest list, sorted by first name,
-- which looks like
-- Total fun: 23924
-- Adam Debergues
-- Adeline Anselme

parse :: GuestList -> String
parse (GL empl fun) = "Fun score :"
  ++ show fun ++ "\n"
  ++ unlines (map empName empl)
  
main :: IO ()
main = do
  file <- readFile "week-08/company.txt"
  putStrLn . parse . maxFun . read $ file
