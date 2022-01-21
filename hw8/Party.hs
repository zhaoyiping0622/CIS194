{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where

import Employee
import Data.Tree
import Data.List

glCons :: Employee -> GuestList -> GuestList
glCons x (GL a b) = GL (x:a) $ empFun x + b

instance Semigroup GuestList where
  (<>) (GL a b) (GL a' b') = GL (a<>a') (b+b')

instance Monoid GuestList where
  mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

treeFold :: (a->b->b) -> b -> Tree a -> b
treeFold f binit (Node x xs) = f x (foldl (treeFold f) binit xs)

combineGLs :: Employee -> [GuestList] -> GuestList
combineGLs e gs
  | empFun e > f = GL [e] $ empFun e
  | otherwise = t
  where t@(GL _ f) = mconcat gs

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e gs = (GL [e] $ empFun e, mconcat $ map (uncurry max) gs) 

maxFun :: Tree Employee -> GuestList
maxFun = uncurry max . maxFun'

maxFun' :: Tree Employee -> (GuestList, GuestList)
maxFun' (Node e xs) = nextLevel e $ map maxFun' xs

printGuestList :: GuestList -> IO()
printGuestList (GL es f) = putStrLn ("Total fun: " ++ show f) >> mapM_ putStrLn (sort (map empName es))

main = readFile "company.txt" >>= (printGuestList . maxFun . read)
