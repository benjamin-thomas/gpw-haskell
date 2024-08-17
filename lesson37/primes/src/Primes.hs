{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

module Primes (sieve1, sieve4, sieve4', primes, isPrime, primeFactors) where

import Data.List (unfoldr)

divBy :: Int -> Int -> Bool
divBy a b = 0 /= mod b a

-- This function is NOT similar to foldr
-- See: https://discourse.haskell.org/t/manual-recursion-performs-better-than-any-fold-why/10176/3
sieve1 :: [Int] -> [Int]
sieve1 [] = []
sieve1 (x : xs) =
  x : sieve1 (filter (divBy x) xs)

sieve4 :: [Int] -> [Int]
sieve4 = unfoldr step
 where
  step [] = Nothing
  step (x : xs) = Just (x, filter (divBy x) xs)

sieve4' :: [Int] -> [Int]
sieve4' lst = unfoldr step lst -- perf: do not ETA reduce
 where
  step [] = Nothing
  step (x : xs) = Just (x, filter (divBy x) xs)

{-
>>> take 5 (sieve1 [2..])
[2,3,5,7,11]

>>> take 5 (sieve4 [2..])
[2,3,5,7,11]

 -}

{-
>>> take 10 primes
[2,3,5,7,11,13,17,19,23,29]
 -}

primes :: [Int]
primes = sieve1 [2 ..]

{- | Returns whether the given number is prime (up to 10_000).

>>> isPrime 13
Just True

>>> isPrime (-3)
Nothing
-}
isPrime :: Int -> Maybe Bool
isPrime n
  | n < 2 = Nothing
  | n > maxPrime = Nothing
  | otherwise =
      Just
        ( -- We must limit the length of the list
          -- Otherwise, checking an even number would go on forever
          -- Also note the first eval is slow, see below
          n `elem` take maxPrime primes
        )
 where
  maxPrime = 10_000

{-
ghci> :set -XNumericUnderscores
ghci> lst = [2..100_000_000::Int]
ghci> :set +s
ghci> length lst
99999999
(5.48 secs, 7,200,554,168 bytes)
ghci> length lst
99999999
(0.30 secs, 554,232 bytes)
 -}

{-
Prime factors for 18:

ghci> take 10 primes
[2,3,5,7,11,13,17,19,23,29]

ghci> 18 `mod` 2
0 => we keep 2
ghci> 18 `div` 2
9 => next number to check against the next prime

ghci> 9 `mod` 3
0 => we keep 3
ghci> 9 `div` 3
3 => would be next number to check against the next prime but recursion stops because we got 0

---

(must limit the number of primes taken!)
>>> unsafePrimeFactors 14 (take 10 primes)
[2,7]

>>> unsafePrimeFactors 13 (take 10 primes)
[13]

>>> unsafePrimeFactors 18 (take 10 primes)
[2,3,3]

 -}
unsafePrimeFactors :: Int -> [Int] -> [Int]
unsafePrimeFactors 0 [] = []
unsafePrimeFactors _ [] = []
unsafePrimeFactors n (x : xs) =
  if n `mod` x == 0
    then
      x : unsafePrimeFactors (n `div` x) (x : xs)
    else
      unsafePrimeFactors n xs

{-

>>> primeFactors 540
Just [2,2,3,3,3,5]

>>> product <$> Just [2,2,3,3,3,5]
Just 540

 -}
primeFactors :: Int -> Maybe [Int]
primeFactors n
  | n < 2 = Nothing
  | n > maximum primes' = Nothing
  | otherwise = Just $ unsafePrimeFactors n primes'
 where
  primes' = take 100 primes
