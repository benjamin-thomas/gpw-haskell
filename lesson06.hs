{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use repeat" #-}
module Lesson06 where

{-  |

Q 6.1 implement repeat

>>> take 5 $ repeat 1
[1,1,1,1,1]

>>> take 5 $ repeat' 2
[2,2,2,2,2]

>>> take 5 $ repeat'' 3
[3,3,3,3,3]

 -}

repeat' :: a -> [a]
repeat' x = x : repeat' x

repeat'' x = cycle [x]

{- | Q6.2: write subseq

>>> subseq 2 5 [1 .. 10]
[3,4,5]

>>> subseq 2 7 "A puppy"
"puppy"
-}
subseq :: Int -> Int -> [a] -> [a]
subseq s e = take (e - s) . drop s

{- | Q6.3: write inFirstHalf

>>> inFirstHalf 1 [1,2,3,4,5,6]
True

>>> inFirstHalf 3 [1,2,3,4,5,6]
True
>>> inFirstHalf 4 [1,2,3,4,5,6]
False
>>> inFirstHalf 99 [1,2,3,4,5,6]
False
-}
inFirstHalf n lst =
    n `elem` half
  where
    half = take (div (length lst) 2) lst
