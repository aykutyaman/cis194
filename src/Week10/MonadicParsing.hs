module MonadicParsing where

-- https://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf

-- import Control.Applicative
import Data.Char

{--
2*3+4

    +
   / \
  *   4
 / \
2   3
--}

-- A parser can be viewed a directly as a function:
-- type Parser = String -> Tree

-- however, a parser might not always consume its entire argument string.
-- So we also return  any uncomsumed part
-- type Parser = String -> (Tree, String)

-- Similarly, a parser might not always succeed. So we return a list of results.
-- (empty list denotes failure)
-- type Parser = String -> [(Tree, String)]

-- Finally different parsers will likely return different kinds of tree, or more generally,
-- any kind of value.
-- type Parser a = String -> [(a, String)]
-- A parser of type a is a function that takes an input string and produces a list of results,
-- each of which is a pair comprising a result value of type a and an output string.

newtype Parser a = Parser (String -> [(a, String)])

-- Parser of this type can then be applied to an input string using a function that simply
-- removes the dummy constructor.

parse :: Parser a -> String -> [(a, String)]
parse (Parser p) inp = p inp

-- Our first parsing primitive is called item, which fails if the input string is empty,
-- and succeeds with the first character as the result value otherwise:

item :: Parser Char
item = Parser (\inp -> case inp of
             [] -> []
             (x:xs) -> [(x, xs)])

-- Sequencing parsers
-- Make the parser type into an instance of the functor, applicative and monad classes, in
-- order that the do notation can then be used to combine parsers in sequence
-- Functor
instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = Parser (\inp -> case parse p inp of
                        [] -> []
                        [(v, out)] -> [(f v, out)])

-- fmap applies a function to the result value of a parser if the parser succeeds, and
-- propagates the failure otherwise
-- > parse (fmap toUpper item) "abc"
-- > parse (fmap toUpper item) ""

-- Applicative Functor
instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = Parser (\inp -> [(v, inp)])

  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = Parser (\inp -> case parse pg inp of
                                [] -> []
                                [(g, out)] -> parse (fmap g px) out)

-- <*> applies a parser that returns a function to a parser that returns an argument to give
-- a parser that returns the result of applying the function to the argument, and only succeeds
-- if all the components suceed. For example, a parser that consumes three characters, discards the
-- second, and returns the first andthrid as a pair:
three :: Parser (Char, Char)
three = pure g <*> item <*> item <*> item
  where g x y z = (x, z)

-- > parse three "abcde" -- [(('a','c'),"de")]
-- > parse three "ab"    -- []

-- Note that the applicative machinery automatically ensures that the above parser fails if
-- the input string is too short, without the need to detect or manage this ourselves.

-- Monad
instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = Parser (\inp -> case parse p inp of
                              [] -> []
                              [(v, out)] -> parse (f v) out)
-- The parser p >>= f fails if the application of the parser p to the input string inp fails,
-- and otherwise applies the function f to the result value v to give another parser f v,
-- which is then applied to the output string out that was produced by the first parser to give
-- the final result
three' :: Parser (Char, Char)
three' = do
  x <- item
  _ <- item
  z <- item
  return (x, z)
-- > parse three "abcde" -- [(('a','c'),"de")]
-- > parse three "ab"    -- []

-- 13.5 Making choices
-- Do notation combines parsers in sequence, with the output string from each parser in the
-- sequence becoming the input string for the next. Another natural way of combining parsers is
-- to apply one parser to the input string, and if this fails to then apply another to the same
-- input instead.

-- Making a choice between two alternatives can be generalised to a range of applicative types.

class Applicative f => Alternative f where
  -- The intuition: empty represents an alternative that has failed
  empty :: f a
  -- an appropriate choice operator for the type
  (<|>) :: f a -> f a -> f a
  many :: f a -> f [a]
  some :: f a -> f [a]

  many x = some x <|> pure []
  some x = pure (:) <*> x <*> many x


{-- laws
empty <|> x = x
x <|> empty = x
x <|> (y <|> z) = (x <|> y) <|> z
--}

instance Alternative Maybe where
  empty = Nothing
  Nothing <|> my = my
  (Just x) <|> _ = Just x

-- The instance for the Parser type is a natural extension of this idea, where
-- empty is the parser that always fails regardless of the input string, and <|>
-- is a choice operator that returns the result of the first parser if it succeeds
-- on the input, and applies the seond parser to the same input otherwise:

instance Alternative Parser where
  -- empty :: Parser a
  empty = Parser (\_ -> [])

  -- <|> :: Parser a -> Parser a -> Parser a
  p <|> q = Parser (\inp -> case parse p inp of
                              [] -> parse q inp
                              [(v, out)] -> [(v, out)])

-- > parse empty "abc"
-- > parse (item <|> return 'd') "abc" -- [('a', "bc")]
-- > parse (empty <|> return 'd') "abc" -- [('d', "abc")]

-- 13.6 Derived primitives
-- We have three basic parsers:
-- `item` consumes a single character if the input string is non-empty
-- `return v` always succeeds with the result value v
-- `empty` always fails
-- In combination with sequencing and choice, these primitives can be used to define
-- a number of other useful parsers.

-- parser for single characters that satisfy a predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do x <- item
               if p x then return x else empty

digit :: Parser Char
digit = satisfy isDigit

lower :: Parser Char
lower = satisfy isLower

upper :: Parser Char
upper = satisfy isUpper

alphanum :: Parser Char
alphanum = satisfy isAlphaNum

char :: Char -> Parser Char
char x = satisfy (== x)

-- > parse (char 'a') "abc" -- [('a', "bc")]

string :: String -> Parser String
string [] = return []
string (x:xs) = do _ <- char x
                   _ <- string xs
                   return (x:xs)

-- The empty string can always be parsed, while for a non-empty string we parse the
-- first character, recursively parse the remaining characters, and return the string
-- as the result value. Note that string only succeeds if the entire target string
-- is consumed from the input.
-- > parse (string "abc") "abcdef" -- [("abc", "def")]

-- > parse (many digit) "123abc" -- [("123", "abc")]
-- > parse (man digit) "abc"     -- [("", "abc")]

-- With many and some we can define parsers for identifiers (variable names)
-- comprising a lower-case letter followed by zero or more alphanumeric characters,
-- natural nubmers comprising one or more digits, and spacing comprising zero or more
-- space, tab, and newline characters:

ident :: Parser String
ident = do x <- lower
           xs <- many alphanum
           return (x:xs)

-- > parse ident "abc def" -- [("abc", " def")]

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

-- > parse nat "123 abc" -- [(123, " abc")]

space :: Parser ()
space = do many (satisfy isSpace)
           return ()

-- > parse space "   abc" -- [((),"abc")]

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
         <|> nat

-- parse int "-123 abc" -- [(-123," abc")]

-- 13.7 Handling spacing
-- Most real-life parsers allow spacing to be freely used around the basic tokens in their input
-- For example 1+2 and 1 + 2 are both valid
-- For this we define a new primitive that ignores any space before and after applying a parser
-- for a token:
token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

ident' :: Parser String
ident' = token ident

nat' :: Parser Int
nat' = token nat

int' :: Parser Int
int' = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

-- Using these primitives a parser for a non-empty list of natural numbers that ignores
-- spacing around tokens can be defined as follows:

nats :: Parser [Int]
nats = do _ <- symbol "["
          n <- nat'
          ns <- many (do
                         _ <- symbol ","
                         nat')
          _ <- symbol "]"
          return (n:ns)

-- parse nats "   [1,2,   3]  " -- [([1,2,3],"")]

-- 13.8 Aritmetic expressions
-- Grammar
-- expr ::= expr + expr | expr * expr | ( expr ) | nat
-- nat  ::= 0 | 1 | 2 | ...

-- 2*3+4 parse tree:
-- the tokens in the expression appear at the leaves, and the grammatical rules applied to
-- construct the expression give rise to the branching structure:

-- expr   ::= term + expr | term
-- term   ::= factor * term | factor
-- factor ::= ( expr ) | nat
-- nat    ::= 0 | 1 | 2 | ...

expr :: Parser Int
expr = do x <- term
          _ <- symbol "+"
          y <- expr
          return (x + y)
          <|> term

term :: Parser Int
term = do f <- factor
          _ <- symbol "*"
          t <- term
          return (f * t)
          <|> factor

factor :: Parser Int
factor = do _ <- symbol "("
            e <- expr
            _ <- symbol ")"
            return e
            <|> nat'

eval :: String -> Int
eval xs = case parse expr xs of
            [(n, [])] -> n
            [(_, out)] -> error ("Unused input " ++ out)

-- > eval "2*3+4" -- 10
-- > eval "2*(3+4)" -- 14
