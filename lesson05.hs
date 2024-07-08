module Lesson05 where

{- | Exercise 5.4

>>> sub2 10
8

>>> sub2' 10
8
-}
sub2 :: (Num a) => a -> a
sub2 n = n - 2

sub2' :: (Num a) => a -> a
sub2' = flip (-) 2
