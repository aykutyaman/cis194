{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
import Control.Applicative (liftA3)
import Data.Maybe (fromMaybe)

-- Exercise 1: How to parse an individual message

mkMessageType :: String -> String -> Maybe MessageType
mkMessageType "I" _ = Just Info
mkMessageType "W" _ = Just Warning
mkMessageType "E" x = Just $ Error (read x :: Int)
mkMessageType _ _   = Nothing

mkTimeStamp :: String -> String -> String -> Maybe Int
mkTimeStamp "I" x _ = Just (read x :: Int)
mkTimeStamp "W" x _ = Just (read x :: Int)
mkTimeStamp "E" _ y = Just (read y :: Int)
mkTimeStamp _ _ _ = Nothing

mkMessageContent :: String -> String -> [String] -> Maybe String
mkMessageContent "E" _ n = Just $ unwords n
mkMessageContent "I" m n = Just $ mappend (m ++ " ") $ unwords n
mkMessageContent "W" m n = Just $ mappend (m ++ " ") $ unwords n
mkMessageContent _ _ _   = Nothing

mkMessage :: [String] -> Maybe LogMessage
mkMessage (x:y:z:tt) =
  liftA3 LogMessage (mkMessageType x y) (mkTimeStamp x y z) (mkMessageContent x z tt)
mkMessage _ = Nothing

parseMessage :: String -> LogMessage
parseMessage m = fromMaybe (Unknown m) (mkMessage $ words m)

-- parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
-- parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"
-- parseMessage "This is not in the right format" == Unknown "This is not in the right format" 

parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert lm Leaf = Node Leaf lm Leaf
insert lm1@(LogMessage _ ts1 _) (Node left lm2@(LogMessage _ ts2 _) right)
  | ts1 >= ts2 = Node left lm2 (insert lm1 right)
  | otherwise = Node (insert lm1 left) lm2 right
insert _ t = t

-- https://bit.ly/3f2uldx
insert' :: LogMessage -> MessageTree -> MessageTree
insert' (Unknown _) t = t
insert' lm Leaf = Node Leaf lm Leaf
insert' m1 (Node left m2 right)
  | m1 >= m2 = Node left m2 (insert' m1 right)
  | otherwise = Node (insert' m1 left) m2 right

-- Exercise 3
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Exercise 4
-- It takes a sorted MessageTree and returns a list of LogMessages it contains,
-- sorted by timestamp from smallest to biggest. (in-order traversal)
-- MessageTree
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left message right) = inOrder left ++ [message] ++ inOrder right

-- Exercise 5
-- relevant: errors with a severity of at least 50
-- It takes an unsorted list of LogMessages, and returns a list of the messages
-- corresponding to any errors with a severity of 50 or greater, sorted by timestamp.
isSevere :: LogMessage -> Bool
isSevere (LogMessage (Error e) _ _) = e >= 50
isSevere _ = False

text :: LogMessage -> String
text (LogMessage _ _ m) = m
text _ = ""

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map text . inOrder . build . filter isSevere
