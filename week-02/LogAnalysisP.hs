module LogAnalysisP where

import Text.ParserCombinators.ReadP
import Log

severityParser :: Char -> ReadP Int
severityParser 'E' = do
  satisfy (== ' ')
  fs <- many1 digit
  satisfy (== ' ')
  return (read fs)
severityParser _ = pfail

digit :: ReadP Char
digit = satisfy (\c -> c >= '0' || c <= '9')

messageChar :: ReadP Char
messageChar = satisfy (\c -> c == 'I' || c == 'E' || c == 'W')

messageTypeParser :: ReadP MessageType
messageTypeParser = do
  messageType <- messageChar

  -- How to use `option` here, or how to achieve the optional error
  -- severity parsing?
  -- errorNumber <- option 0 (severityParser messageType)
  errorNumber <- severityParser messageType

  return (case messageType of
            'I' -> Info
            'E' -> Error errorNumber
            'W' -> Warning
            _ -> error "It should not happen"
         )

messageParser :: ReadP (MessageType, Int)
messageParser = do
  messageType <- messageTypeParser
  timestamp <- many1 digit
  return (messageType, read timestamp)

parseLogMessage :: ReadP (MessageType, TimeStamp) -> String -> LogMessage
parseLogMessage parser input =
  case readP_to_S parser input of
    [] -> Unknown input
    (((messageType, timestamp), content):_) ->
      LogMessage messageType timestamp content

-- parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
-- parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"
-- parseMessage "W 5 Flange is" == LogMessage Warning 5 "Flange is"
-- parseMessage "This is not" == Unknown "This is not in the right format"
-- parseMessage :: String -> LogMessage
parseMessage :: String -> LogMessage
parseMessage= parseLogMessage messageParser
