module Lesson07 where

{- |

>>> take 3 [1..]
[1,2,3]

>>> take' 3 [1..]
[1,2,3]
-}
take' :: (Ord t, Num t) => t -> [a] -> [a]
take' _ [] = []
take' n (x : xs) =
    if n <= 0
        then []
        else x : take' (n - 1) xs

{- | Euclid's GCD algorithm

>>> gcd' 20 16
4

>>> gcd'' 20 16
4
-}
gcd' :: (Integral n) => n -> n -> n
gcd' a b =
    if remain == 0
        then b
        else gcd' b remain
  where
    remain = a `mod` b

gcd'' :: (Integral n) => n -> n -> n
gcd'' a 0 = a
gcd'' a b = gcd'' b (a `mod` b)
