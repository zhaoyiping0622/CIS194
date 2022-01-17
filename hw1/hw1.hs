import Distribution.SPDX (LicenseId(AFL_3_0))
toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x<=0 = []
  | otherwise = mod x 10 : toDigitsRev (div x 10)

toDigits :: Integer -> [Integer]
toDigits x
  | x<=0 = []
  | otherwise =  toDigits (div x 10) ++ [mod x 10]

listLength :: [x] -> Integer
listLength [] = 0
listLength (_:xs) = 1+listLength xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther x@(a:xs) 
  | even (listLength xs) = a : doubleEveryOtherEven xs
  | otherwise = doubleEveryOtherEven x
  where
    doubleEveryOtherEven :: [Integer] -> [Integer]
    doubleEveryOtherEven [] = []
    doubleEveryOtherEven [x] = undefined 
    doubleEveryOtherEven (a:b:xs) = a*2:b:doubleEveryOtherEven xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sumInteger x + sumDigits xs
  where 
    sumInteger :: Integer -> Integer
    sumInteger x
      | x <= 0 = 0
      | otherwise = mod x 10 + sumInteger (div x 10)

validate :: Integer -> Bool
validate x = mod (sumDigits (doubleEveryOther(toDigits x))) 10 == 0

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined
