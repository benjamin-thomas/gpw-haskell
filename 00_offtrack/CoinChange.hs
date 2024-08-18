{-# OPTIONS_GHC -Wall #-}

module CoinChange where

import Data.List (unfoldr)

{-

After having read the problem, I've actually implemented a "greedy" algorithm (in change1 and change2).
Apart from not guaranteeing that amt finishes at 0, it may give the wrong solution.

I should look into so-called "dynamic" programming at some point, see: https://betterprogramming.pub/learn-dynamic-programming-the-coin-change-problem-22a104478f50

 -}

{-

>>> change1 87 [25, 10, 5, 1]
[25,25,25,10,1,1]

>>> sum $ change1 87 [25, 10, 5, 1]
87

 -}

change1 :: (Ord a, Num a) => a -> [a] -> [a]
change1 _ [] = []
change1 amt (x : xs) =
    if amt >= x
        then x : change1 (amt - x) (x : xs)
        else change1 amt xs

{-

>>> change2 87 [25, 10, 5, 1]
[25,25,25,10,1,1]

>>> sum $ change2 87 [25, 10, 5, 1]
87

 -}
change2 :: (Ord a, Num a) => a -> [a] -> [a]
change2 amount coins = unfoldr step (amount, coins)
  where
    step (_, []) = Nothing
    step (amt, x : xs)
        | amt >= x = Just (x, (amt - x, x : xs))
        | otherwise = step (amt, xs)
