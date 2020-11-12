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

