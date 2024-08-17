{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Primes (isPrime, PrimeError) where

import Data.Char (isDigit)

import Control.Monad (foldM)
import Prelude hiding (last, succ, tail)

data PrimeError
    = TooLarge
    | TooSmall

instance Show PrimeError where
    show TooLarge = "Value exceeds max bound"
    show TooSmall = "Value must be greater than 1"

primesData :: (Int, [Int])
primesData =
    ( 10
    , [2, 3, 5, 7]
    )

maxN :: Int
maxN = fst primesData

primes :: [Int]
primes = snd primesData

isPrime :: Int -> Either PrimeError Bool
isPrime n
    | n < 2 = Left TooSmall
    | n > maxN = Left TooLarge
    | otherwise = Right $ n `elem` primes

{- Q38.1

>>> addStrInts "a" "2"
Left "First value can't be parsed"

>>> addStrInts "1" "b"
Left "Second value can't be parsed"

>>> addStrInts "10" "20"
Right 30
 -}
addStrInts :: String -> String -> Either String Int
addStrInts a b =
    case (all isDigit a, all isDigit b) of
        (False, _) -> Left "First value can't be parsed"
        (_, False) -> Left "Second value can't be parsed"
        (True, True) -> Right $ read a + read b

{- Q38.2

ghci> maxBound :: Int
9223372036854775807

ghci> succ (9223372036854775805 :: Int)
9223372036854775806

ghci> (succ . succ) (9223372036854775805 :: Int)
9223372036854775807

ghci> (succ . succ . succ) (9223372036854775805 :: Int)
\*** Exception: Prelude.Enum.succ{Int}: tried to take `succ' of maxBound

---

>>> succ (9223372036854775805 :: Int)
Just 9223372036854775806
>>> succ =<< succ (9223372036854775805 :: Int)
Just 9223372036854775807

>>> succ =<< succ =<< succ (9223372036854775805 :: Int)
Nothing

 -}
-- succ :: Maybe a
succ :: (Ord a, Bounded a, Num a) => a -> Maybe a
succ x =
    if x >= maxBound
        then Nothing
        else Just $ x + 1

{-

>>> testSucc 0
Just 9223372036854775800

>>> testSucc 1
Just 9223372036854775801

>>> testSucc 7
Just 9223372036854775807

>>> testSucc 8
Nothing

---

ghci> mapM_ print $ map testSucc [0..10]
Just 9223372036854775800
Just 9223372036854775801
Just 9223372036854775802
Just 9223372036854775803
Just 9223372036854775804
Just 9223372036854775805
Just 9223372036854775806
Just 9223372036854775807
Nothing
Nothing
Nothing

 -}

testSucc :: Int -> Maybe Int
testSucc end =
    let start = (9223372036854775800 :: Int)
     in foldM (\acc _time -> succ acc) start [1 :: Int .. end]

{-

ghci> tail []
\*** Exception: Prelude.tail: empty list

>>> tail [1,2,3]
Just [2,3]

>>> tail =<< tail [1,2,3]
Just [3]

>>> tail =<< tail =<< tail [1,2,3]
Just []

>>> tail =<< tail =<< tail =<< tail [1,2,3]
Nothing

 -}
tail :: [a] -> Maybe [a]
tail [] = Nothing
tail (_ : xs) = Just xs

{-
>>> last []
Left "empty list!"

>>> last [1,2,3]
Right 3

>>> last [1..9999]
Left "This is dumb! Do something else!!"

 -}
last :: [a] -> Either String a
last [] = Left "empty list!"
last (_ : xs) = go xs 1000
  where
    go :: [a] -> Int -> Either String a
    go [] _ = error "Impossible"
    go _ 0 = Left "This is dumb! Do something else!!"
    go [one] _ = Right one
    go (_ : ys) n = go ys (n - 1)
