{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where
import           Control.Applicative

import           Data.Char

import Test.QuickCheck

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-- Exercise 1
-- Implement a Functor instance for Parser

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f pa = Parser (\s -> case runParser pa s of
                              Nothing -> Nothing
                              Just(n, out) -> Just (f n, out))

-- runParser ((+1) <$> posInt) "999ali" == Just (1000, "ali")
-- TODO: quickCheck functor laws

-- Exercise 2
-- Implement an Applicative instance for Parser
instance Applicative Parser where
  -- pure :: a -> Parser a
  pure x = Parser (\s -> Just(x, s))

  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> pa = Parser (\s -> case runParser pf s of
                                      Nothing -> Nothing
                                      Just (f, out) ->
                                        runParser (fmap f pa) out)

-- runParser (pure (+1) <*> posInt) "123ada" == Just (124,"ada")

-- Exercise 3
-- Create a parser abParser which expects to see the characters 'a' and 'b'
-- and returns them as a pair
abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'
-- abParser = liftA2 (,) (char 'a') (char 'b')

-- runParser abParser "abcdef" == Just(('a', 'b'), "cdef")

-- Create a parser abParser_ which acts in the same way as abParser but
-- returns () instead of the characters 'a' and 'b'
abParser_ :: Parser ()
abParser_ = const . const () <$> char 'a' <*> char 'b'

-- Create a parser intPair which reads two integer values separated by
-- a space and returns the integer values in a list. 
intPair :: Parser [Integer]
intPair = (\x _ y -> [x, y]) <$> posInt <*> char ' ' <*> posInt

-- Exercise 4
-- Write an Alternative instance for Parser
instance Alternative Parser where
  -- empty :: Parser a
  empty = Parser $ const Nothing

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p1 <|> p2 = Parser (\s -> case runParser p1 s of
                              Nothing -> runParser p2 s
                              Just t -> Just t)

-- runParser ((char 'a') <|> (char 'b')) "ahel" == Just ('a', "hel")
-- runParser ((char 'a') <|> (char 'b')) "bel" == Just ('b', "el")
-- runParser ((char 'a') <|> (char 'b')) "kel" == Nothing

-- Exercise 5
-- Implement a parser intOrUppercase which parses either an integer value
-- or an uppercase character, and fails otherwise.
intOrUppercase :: Parser ()
intOrUppercase = (() <$ posInt) <|> (() <$ satisfy isUpper)

-- runParser intOrUppercase "342abcd" == Just((), "abcd")
-- runParser intOrUppercase "XYZ" == Just((), "YZ")
-- runParser intOrUppercase "foo" == Nothing
