{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

parseMessageType :: Char -> String -> MessageType
parseMessageType 'I' _ = Info
parseMessageType 'W' _ = Warning
parseMessageType 'E' x = Error ((read (head (words x))) :: Int)

parseTimestamp :: String => TimeStamp
parseTimestamp x = read (head (words x)) :: TimeStamp

parseMessage :: String -> LogMessage
parseMessage ('I':' ':xs) = LogMessage (parseMessageType 'I' xs) (parseTimestamp xs) (unwords (drop 1 (words xs)))
parseMessage ('E':' ':xs) = LogMessage (parseMessageType 'E' xs) (parseTimestamp (unwords (tail (words xs)))) (unwords (drop 2 (words xs)))
parseMessage ('W':' ':xs) = LogMessage (parseMessageType 'W' xs) (parseTimestamp xs) (unwords (drop 1 (words xs)))
parseMessage x        = Unknown x

parse :: String -> [LogMessage]
parse x
    | length (lines x) == 1 = [parseMessage x]
    | otherwise = parseMessage (head (lines x)) : parse (unlines (tail (lines x)))

insert :: LogMessage -> MessageTree -> MessageTree
insert (LogMessage Unknown x) tree = tree
insert x (Leaf) = Node Leaf x
insert (LogMessage Error x y z) (Node ltree LogMessage x y x rtree)
    | y >= 
