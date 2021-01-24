module ParserCombinators where

import Data.Char

newtype Parser a = Parser (String -> [(a, String)])

getc :: Parser Char
getc = Parser (\inp -> case inp of
                  [] -> []
                  (x:xs) -> [(x,xs)])

apply :: Parser a -> String -> [(a, String)]
apply (Parser p) s = p s

parse :: Parser a -> String -> a
parse p = fst . head . (apply p)

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f pa = Parser (\s -> case apply pa s of
                              [] -> []
                              [(v, out)] -> [(f v, out)])

instance Applicative Parser where
  -- pure :: a -> f a
  pure x = Parser (\s -> [(x, s)])

  -- (<*>) :: f (a -> b) -> f a -> f b
  fab <*> fa = Parser (\s -> case apply fab s of
                         [] -> []
                         [(g, out)] -> apply (fmap g fa) out)
                         
instance Monad Parser where
  return x = Parser (\s -> [(x, s)])
  p >>= q = Parser  (\s -> case apply p s of
                             []         -> []
                             [(v, out)] -> apply (q v) out)


sat :: (Char -> Bool) -> Parser Char
sat p = do
  c <- getc
  if p c then return c else empty

empty :: Parser a
empty = Parser $ const []

char :: Char -> Parser Char
char x = sat (==x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do
  char x
  string xs
  return (x:xs)

lower :: Parser Char
lower = sat isLower

digit :: Parser Int
digit = do
  d <- sat isDigit
  return (fromEnum d - fromEnum '0')

(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = Parser (\s -> case apply p s of
                     [] -> apply q s
                     [(v, out)] -> [(v, out)])

lowers :: Parser String
lowers = do
  c <- lower
  cs <- lowers
  return (c:cs)
  <|> return ""

addition :: Parser Int
addition = do
  m <- digit
  char '+'
  n <- digit
  return (m+n)

best = digit >>= rest
rest m = do
  char '+'
  n <- digit
  return (m+n)
  <|> return m

none = return []

many :: Parser a -> Parser [a]
many p = do
  x <- p
  xs <- many p
  return (x:xs)
  <|> none

space :: Parser ()
space = many (sat isSpace) >> return ()

symbol :: String -> Parser String
symbol xs = space >> string xs

token :: Parser a -> Parser a
token p = space >> p

some :: Parser a -> Parser [a]
some p = do
  x <- p
  xs <- many p
  return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (foldl1 shiftl xs)
           where shiftl m n = 10*m+n

natural :: Parser Int
natural = token nat
  
int :: Parser Int
int = do
  symbol "-"
  n <- natural
  return (-n)
  <|> natural
