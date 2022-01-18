module Golf where

skips :: [a] -> [[a]]
skips x = map (\ y -> g x y y) [1 .. length x]
  where
    g :: [a] -> Int -> Int -> [a]
    g [] _ _ = []
    g (t:s) y z
      | z==1 = t:g s y y
      | otherwise = g s y (z-1)

get1 :: (a,b,c) -> a
get1 (a,_,_) = a

getAnd23 :: (a,Bool,Bool) -> Bool
getAnd23 (_,b,c) = b&&c

localMaxima :: [Integer] -> [Integer]
localMaxima x = map get1 
  $ filter getAnd23 
  $ zip3 x (False:cmp x) (reverse (False:cmp (reverse x)))
  where 
    cmp::[Integer]->[Bool]
    cmp []=[]
    cmp [_]=[]
    cmp (a:y@(b:xs))=(a<b):cmp y

draw1 :: [Int] -> Int -> String
draw1 x y = map g x ++ "\n"
  where 
    g :: Int -> Char
    g x
      | x>=y = '*'
      | otherwise = ' '

draw :: [Int] -> String
draw x 
  | m == 0 = "==========\n0123456789\n"
  | otherwise = draw1 x m ++ draw (map (\x -> min x (m-1)) x)
  where
    m = foldl max 0 x

histogram :: [Integer] -> String
histogram x = draw $ map (\a -> length $ filter (==a) x) [0..9]
