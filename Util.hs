module Util where

import Data.Bits
import Data.Numbers.Primes

log2 :: Integer -> Int
log2 1 = 0
log2 n = 1 + log2 (n `div` 2)

bits' :: Int -> Integer -> [Integer]
bits' 0 n = [n]
bits' k n
    | 2^k > n   = 0 : bits' (k-1) n
    | otherwise = 1 : bits' (k-1) (n - 2^k)

bits :: Integer -> [Integer]
bits n = bits' (log2 n) n

bits'' :: Integer -> [Integer]
bits'' 0 = []
bits'' n = n .&. 1 : bits'' (n `shiftR` 1)

modSquares :: Int -> Integer -> [Integer] -> [Integer]
modSquares 0 m ss = ss
modSquares n m (s:ss) = modSquares (n-1) m $ (s*s `mod` m) : s:ss

modSquares' :: Integer -> Integer -> [Integer]
modSquares' m n = x : modSquares' m (x*x)
    where x = n `mod` m

modExp :: Integer -> Integer -> Integer -> Integer
modExp m b e = product (zipWith (^) bs es) `mod` m
--    where es = bits e
--          bs = modSquares (log2 e) m [b `mod` m]
    where es = bits'' e
          bs = modSquares' m b

groups :: Eq a => [a] -> [(a,Integer)]
groups [] = []
groups (x:xs) = (x,c) : groups xss
    where c = toInteger $ 1 + length (takeWhile (==x) xs)
          xss = dropWhile (==x) xs

totient' :: (Integer, Integer) -> Integer
totient' (b,e) = (b-1) * b^(e-1)

carm' :: (Integer, Integer) -> Integer
carm' (b,e)
    | b == 2 && e >= 3 = totient' (b,e) `div` 2
    | otherwise        = totient' (b,e)

carm :: Integer -> Integer
carm = foldr lcm 1 . map carm' . groups . primeFactors

carm'' :: Integer -> Integer -> Integer
carm'' a b = (a-1) * (b-1)

coprime :: Integer -> Integer -> Bool
coprime a b = gcd a b == 1

eucExt' :: (Integer, Integer, Integer) -> (Integer, Integer, Integer) -> (Integer, Integer, Integer)
eucExt' (r,s,t) (0,_,_) = (r,s,t)
eucExt' (r1,s1,t1) (r2,s2,t2) = eucExt' (r2,s2,t2) next
    where next = ( r1 - q*r2
                 , s1 - q*s2
                 , t1 - q*t2
                 )
          q = r1 `div` r2

eucExt :: Integer -> Integer -> (Integer, Integer, Integer)
eucExt a b = eucExt' (a,1,0) (b,0,1)

mmi :: Integer -> Integer -> Integer
mmi m a = (x + m) `mod` m
    where (_,x,_) = eucExt a m

fromStr :: String -> Integer
fromStr "" = 0
fromStr (x:xs) = toInteger (fromEnum x) + 256 * fromStr xs

toStr :: Integer -> String
toStr 0 = ""
toStr n = toEnum (fromInteger m) : toStr d
    where (d,m) = n `divMod` 256