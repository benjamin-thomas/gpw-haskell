{-# LANGUAGE LambdaCase #-}

module Main where

import Primes (PrimeError, isPrime)

displayResult :: Either PrimeError Bool -> String
displayResult = \case
  Right True -> "It's a prime!"
  Right False -> "It's composite!"
  Left primeError -> show primeError

main :: IO ()
main = do
  putStrLn "Enter a number to test for primality:"
  n <- read <$> getLine
  let result = isPrime n
  putStrLn $ displayResult result
