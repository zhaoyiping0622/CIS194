{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE FlexibleInstances #-}


module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = map fst $ iterate (\(a,b) -> (b,a+b)) (0,1)

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x:streamToList xs

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons y (streamFromSeed f y)
  where y = f x 

nats :: Stream Integer 
nats = streamFromSeed (+ 1) 0

ruler :: Stream Integer
ruler = streamMap f nats
  where 
    f :: Integer -> Integer
    f x
      | odd x = 0
      | otherwise = 1 + f (div x 2)

x :: Stream Integer
x = Cons 0 $ Cons 1 $ streamRepeat 0

instance Num (Stream Integer) where
  fromInteger x = Cons x $ streamRepeat 0
  negate (Cons x xs) = Cons (-x) $ negate xs
  (+) (Cons a as) (Cons b bs) = Cons (a+b) (as+bs)
  (*) (Cons a as) b@(Cons b' bs) = Cons (a*b') (streamMap (*a) bs +as*b)

(/) :: Stream Integer -> Stream Integer -> Stream Integer
(/) (Cons a as) (Cons b bs) = q
  where q = streamMap (`div` b) (Cons a (as-q*bs))

fibs3 :: Stream Integer
fibs3 = x Fibonacci./ (1-x-x^2)

newtype Matrix = M ((Integer,Integer),(Integer,Integer))

getElem :: Integer -> Integer -> Matrix -> Integer
getElem 0 0 (M ((a,_),_)) = a
getElem 0 1 (M ((_,a),_)) = a
getElem 1 0 (M (_,(a,_))) = a
getElem 1 1 (M (_,(_,a))) = a
getElem _ _ _ = undefined 

instance Num (Matrix) where
  (*) (M ((a,b),(c,d))) (M ((a',b'),(c',d'))) = M ((a*a'+b*c',a*b'+b*d'),(c*a'+d*c',c*b'+d*d'))

fib4 :: Integer -> Integer
fib4 x = getElem 0 0 (M ((1,1),(1,0)) ^ x)
