{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

parseMessage :: String -> LogMessage
parseMessage x = parseMessage' (words x)
  where
    parseMessage' :: [String] -> LogMessage 
    parseMessage' ("I":ts:xs) = LogMessage Info (read ts) (unwords xs)
    parseMessage' ("W":ts:xs) = LogMessage Warning (read ts) (unwords xs)
    parseMessage' ("E":lv:ts:xs) = LogMessage (Error (read lv)) (read ts) (unwords xs)
    parseMessage' t = Unknown (unwords t)

parse :: String -> [LogMessage]
parse xx = parse' (lines xx)
  where
    parse' :: [String] -> [LogMessage]
    parse' [] = []
    parse' (x:xs) = parseMessage x : parse' xs

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) x = x
insert x Leaf = Node Leaf x Leaf
insert x@(LogMessage _ ts1 _) (Node lt z@(LogMessage _ ts2 _) rt) 
  | ts1 <= ts2 = Node (insert x lt) z rt
  | ts1 > ts2 = Node lt z (insert x rt)
insert _ _ = undefined 

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lt x rt) = inOrder lt ++ [x] ++ inOrder rt

sort :: [LogMessage] -> [LogMessage]
sort x = inOrder $ build x

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong x = map getMessage $ sort $ filter errorGreater50 x
  where 
    getMessage :: LogMessage -> String
    getMessage (LogMessage _ _ xx) = xx
    getMessage _ = undefined
    errorGreater50 :: LogMessage -> Bool
    errorGreater50 (LogMessage (Error xx) _ _)
      | xx>=50 = True
    errorGreater50 _ = False
