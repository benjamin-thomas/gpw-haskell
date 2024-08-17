{-# LANGUAGE NumericUnderscores #-}

import Control.Monad (when)
import Data.Maybe (isJust, isNothing)
import qualified Perf
import Primes (isPrime, primeFactors)
import Test.QuickCheck (
  Args (maxSuccess),
  quickCheck,
  quickCheckWith,
  stdArgs,
 )

prop_validPrimesOnly :: Int -> Bool
prop_validPrimesOnly val =
  if val < 2 || val > 10_000
    then isNothing result
    else isJust result
 where
  result = isPrime val

prop_primesArePrime :: Int -> Bool
prop_primesArePrime val =
  case isPrime val of
    Just True -> null divisors
    _ -> True
 where
  divisors = filter (\x -> val `mod` x == 0) [2 .. (val - 1)]

prop_nonPrimesAreComposite :: Int -> Bool
prop_nonPrimesAreComposite val =
  case isPrime val of
    Just False -> (not . null) divisors
    _ -> True
 where
  divisors =
    filter (\x -> val `mod` x == 0) [2 .. (val - 1)]

prop_factorsSumToOriginal :: Int -> Bool
prop_factorsSumToOriginal val =
  case primeFactors val of
    Just f -> val == product f
    Nothing -> True

prop_allFactorsArePrime :: Int -> Bool
prop_allFactorsArePrime val =
  case primeFactors val of
    Just f -> all (\x -> Just True == isPrime x) f
    Nothing -> True

main :: IO ()
main = do
  when testPerf Perf.main
  quickCheck prop_validPrimesOnly
  quickCheckWith stdArgs{maxSuccess = 1000} prop_primesArePrime
  quickCheckWith stdArgs{maxSuccess = 1000} prop_nonPrimesAreComposite
  quickCheck prop_factorsSumToOriginal
  quickCheck prop_allFactorsArePrime
 where
  testPerf :: Bool
  testPerf = True