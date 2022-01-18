import Data.List (sort,nub)
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs
fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x-2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n 
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)
fun2' = sum . filter even . takeWhile (/=1) . iterate (\n -> if even n then n `div` 2 else 3*n+1)

data Tree a = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

getDeep :: Tree a -> Integer
getDeep Leaf = -1
getDeep (Node x _ _ _) = x

foldTree :: Ord a => [a] -> Tree a
foldTree = foldTree' . sort

setLeftTree :: Tree a -> Tree a -> Tree a
setLeftTree l Leaf = l
setLeftTree Leaf r = r
setLeftTree l (Node rh _ a r) = Node (max (getDeep l + 1) rh) l a r

getRightTree :: [a] -> Tree a
getRightTree [] = Leaf
getRightTree [x] = Node 0 Leaf x Leaf
getRightTree (x:xs) = Node (getDeep rt + 1) Leaf x rt
  where
    rt = foldTree' xs

foldTree' :: [a] -> Tree a
foldTree' [] = Leaf
foldTree' [x] = Node 0 Leaf x Leaf
foldTree' x = setLeftTree (foldTree' $ take llen x) (getRightTree $ drop llen x)
  where 
    llen = div (length x) 2

xor :: [Bool] -> Bool
xor = foldl (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y-> f x:y) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base = foldr (flip f) base . reverse 

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 1+2*x) $ filter (`notElem` toRemove) [1..n]
  where
    toRemove = nub $ sort [i+j+2*i*j| i<-[1..n],j<-[i..n],i+j+2*i*j<=n]
