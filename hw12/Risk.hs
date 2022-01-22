{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving (Show)

battle :: Battlefield -> Rand StdGen Battlefield
battle b@(Battlefield at de) = atr >>= (\x -> der >>= \y -> return (battle' b x y))  
  where atr = replicateM (min at 3 - 1) die
        der = replicateM (min de 2) die

        battle' :: Battlefield -> [DieValue] -> [DieValue] -> Battlefield
        battle' (Battlefield at de) a b = Battlefield at' de'
          where results = zipWith (>) (sort a) (sort b)
                at' = at - length (filter (==False) results)
                de' = de - length (filter id results)
end :: Battlefield -> Bool
end (Battlefield at de) = at <= 1 || de == 0

invade :: Battlefield -> Rand StdGen Battlefield
invade b = battle b >>= (\b' -> if end b' then return b' else invade b')

successProb' :: Int -> Battlefield -> Rand StdGen Double
successProb' n b = replicateM n (invade b >>= (\b -> return (defenders b==0))) >>= (return . (/ n') . fromIntegral . length . filter id)
  where 
    n'::Double
    n' = fromIntegral n

successProb = successProb' 1000
