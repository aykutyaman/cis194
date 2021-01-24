module Parsing where

type Expected = String
type Encountered = String

data ParseError = ParseError Expected Encountered

newtype Parser a = Parser {
  runParser :: String -> (String, Either ParseError a)
}

anyy :: Parser Char
anyy = Parser $ \input -> case input of
  -- some input left: we unpack and return the first character
  (x:xs) -> (xs, Right x)
  -- no input left: the parser fails
  [] -> ("", Left $ ParseError
          "any character"        -- expected
          "the end of the input" -- encountered
        )

eof :: Parser ()
eof = Parser $ \input -> case input of
  -- no input left: the parser succeeds
  [] -> ("", Right ())
  -- leftover data: the parser fails
  (c:_) -> (input, Left $ ParseError
           "the end of the input" -- expected
           [c]                    -- encountered
           )
