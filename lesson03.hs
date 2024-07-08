module Lesson03 where

{-

Listing 3.1
===========

We can effectively replace the `where` clause with lambdas:

(\a b -> if a > b then a else b) => max

 -}

{- |

>>> compute 2 3
25
-}
compute :: (Num a, Ord a) => a -> a -> a
compute x y =
  max sumSquare squareSum
 where
  sumSquare = x ^ 2 + y ^ 2
  squareSum = (x + y) ^ 2

{- | >>> compute' 2 3
25
-}
compute' :: (Num a, Ord a) => a -> a -> a
compute' x y =
  max
    (x ^ 2 + y ^ 2)
    ((x + y) ^ 2)

{- Exercise 3.2

Rewrite with lambda

>>> doubleDouble 2
8

>>> doubleDouble' 2
8
 -}

doubleDouble :: (Num a) => a -> a
doubleDouble x = dubs * 2
 where
  dubs = x * 2

doubleDouble' :: (Num a) => a -> a
doubleDouble' x = (x * 2) * 2

{- |

Overriding values with successive let expressions

>>> n
4

>>> n'
4
-}
n =
  let
    x = 1
   in
    let
      x = 2
     in
      let x = 3 in 4

-- Equivalent to:
n' =
  (\x -> (\x -> (\x -> 4) 3) 2) 1
