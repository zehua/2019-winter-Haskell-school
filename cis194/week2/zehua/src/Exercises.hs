module Exercises where

import Text.Read
import Log

-- ex1
parseInt :: String -> Maybe TimeStamp
parseInt = readMaybe

createLogMessage :: MessageType -> String -> [String] -> Maybe LogMessage
createLogMessage mt ts msg =
  case parseInt ts of
    Just t -> Just . LogMessage mt t . unwords $ msg
    Nothing -> Nothing

parseWords :: [String] -> Maybe LogMessage
parseWords ("I":t:xs)   = createLogMessage Info t xs
parseWords ("W":t:xs)   = createLogMessage Warning t xs
parseWords ("E":e:t:xs)
    | errno >= 1 && errno <= 100 = createLogMessage (Error errno) t xs
    | otherwise                  = Nothing
  where
    errno = read e :: Int
parseWords _            = Nothing

parseMessage :: String -> LogMessage
parseMessage s =
  case parseWords . words $ s of
    Just t  -> t
    Nothing -> Unknown s

parse :: String -> [LogMessage]
parse = fmap parseMessage . lines

-- ex2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert lm Leaf = Node Leaf lm Leaf
insert lm@(LogMessage _ ts _ ) (Node l lm1@(LogMessage _ ts1 _) r)
  | ts < ts1 = Node (insert lm l) lm1 r
  | otherwise = Node l lm1 (insert lm r)
