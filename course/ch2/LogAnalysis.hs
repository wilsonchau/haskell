{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

parseMessageType :: Char -> String -> MessageType
parseMessageType 'I' _ = Info
parseMessageType 'W' _ = Warning
parseMessageType 'E' x = Error ((read (head (words x))) :: Int)
parseMessageType x y = Info

parseTimestamp :: String => TimeStamp
parseTimestamp x = read (unwords (tail (take 2 (words x)))) :: TimeStamp

parseMessage :: String -> LogMessage
parseMessage ('I':' ':xs) = LogMessage (parseMessageType 'I' xs) (parseTimestamp xs) (unwords (drop 2 (words xs)))
parseMessage ('E':' ':xs) = LogMessage (parseMessageType 'E' xs) (parseTimestamp xs) (unwords (drop 2 (words xs)))
parseMessage ('W':' ':xs) = LogMessage (parseMessageType 'W' xs) (parseTimestamp xs) (unwords (drop 2 (words xs)))
parseMessage x        = Unknown x
parseMessage (x:' ':xs)

parseParts :: [String] => LogMessage
parseParts ('I':' ':y:xs)

parse :: String -> [LogMessage]
parse x
    | length (lines x) == 1 = [parseMessage x]
    | otherwise = parseMessage (head (lines x)) : parse (unlines (tail (lines x)))
