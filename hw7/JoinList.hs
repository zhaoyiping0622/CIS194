{-# LANGUAGE FlexibleInstances #-}
module JoinList where

import Sized
import Scrabble
import Buffer
import Editor (runEditor)

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty 
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a <> tag b) a b

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing 
indexJ 0 (Single _ a) = Just a
indexJ _ (Single _ _) = Nothing 
indexJ i (Append m x y) 
  | i < lsize = indexJ i x
  | otherwise = indexJ (i-lsize) y
  where 
    lsize = getSize $ size $ tag x

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n x 
  | n <= 0 = x
  | n >= getSize (size $ tag x) = Empty
dropJ _ Empty = Empty
dropJ _ (Single _ _) = Empty
dropJ n (Append m x y) 
  | n <= lsize = dropJ n x +++ y
  | otherwise = Empty +++ dropJ (n-lsize) y
  where lsize = getSize $ size $ tag x 

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n x
  | n <= 0 = Empty
  | n >= getSize (size $ tag x) = x
takeJ _ Empty = Empty
takeJ _ x@(Single _ _) = x
takeJ n (Append m x y)
  | n <= lsize = takeJ n x
  | otherwise = x +++ takeJ (n-lsize) y
  where lsize = getSize $ size $ tag x 

scoreLine :: String -> JoinList Score String
scoreLine x = Single (scoreString x) x

calM :: String -> (Score, Size)
calM x = (scoreString x, Size 1)

instance Buffer (JoinList (Score, Size) String) where
  toString Empty = []
  toString (Single _ x) = x
  toString (Append _ x y) = toString x ++ "\n" ++ toString y

  fromString = fromStrings . lines 
    where 
      fromStrings :: [String] -> JoinList (Score, Size) String
      fromStrings [] = Empty
      fromStrings [x] = Single (calM x) x
      fromStrings x = l +++ r
        where 
          len = length x
          llen = div len 2
          l = fromStrings $ take llen x
          r = fromStrings $ drop llen x
  
  line = indexJ

  replaceLine n s b 
    | n < 0 || n >= numLines b = b
    | otherwise = takeJ (n-1) b +++ Single (calM s) s +++ dropJ n b

  numLines = getSize . size . snd . tag

  value = getScore . fst . tag

