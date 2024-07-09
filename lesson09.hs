{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use sum" #-}
{-# HLINT ignore "Use product" #-}
{-# HLINT ignore "Use concat" #-}
{-# HLINT ignore "Use null" #-}
module Lesson09 where

import Data.Char (toLower)

{- |

>>> sum5
15

>>> sum5'
15

>>> sub5
-15

>>> sub5'
3
-}

-- 15 = 0 + 1 + 2 + 3 + 4 + 5
sum5 :: Int
sum5 = foldl (+) 0 [1, 2, 3, 4, 5]

-- -15 = 0 - 1 - 2 - 3 - 4 - 5
sub5 :: Int
sub5 = foldl (-) 0 [1, 2, 3, 4, 5]

-- 15 = 1 + (2 + (3 + (4 + (5 + 0))))
sum5' :: Int
sum5' = foldr (+) 0 [1, 2, 3, 4, 5]

-- 3 = 1 - (2 - (3 - (4 - (5 - 0))))
sub5' :: Int
sub5' = foldr (-) 0 [1, 2, 3, 4, 5]

{- |

>>> product' [2,4,6]
48
-}
product' :: [Int] -> Int
product' = foldl (*) 1

{- |
>>> concat' ["hello", "-", "world"]
"hello-world"
-}
concat' :: [String] -> String
concat' = foldl (++) ""

sumOfSquares :: (Num a) => [a] -> a
sumOfSquares lst = foldl (+) 0 $ map (^ 2) lst

{- |

>>> rcons [2,3] 1
[1,2,3]
-}
rcons :: [a] -> a -> [a]
-- rcons xs x = x : xs
rcons = flip (:)

{- |

(rcons (rcons (rcons [] 'a') 'b') 'c')
[] `rcons` 'a' `rcons` 'b' `rcons` 'c'
>>> rev "abc"
"cba"
-}
rev :: [a] -> [a]
-- rev = foldl rcons []
rev = foldl (flip (:)) []

{- | Q9.1 Use `filter` and `length` to re-create `elem`

>>> elem' 1 [1..3]
True

>>> elem' 4 [1..3]
False
-}
elem' :: (Eq a) => a -> [a] -> Bool
elem' x xs = 0 < length (filter (x ==) xs)

{- | Q9.2 implement isPalindrome..ish

>>> isPalindrome "A man a plan a canal Panama"
True

>>> isPalindrome "wat"
False
-}
isPalindrome :: String -> Bool
isPalindrome str =
    normalized == reverse normalized
  where
    normalized = map toLower (filter (/= ' ') str)

{- | Q9.3 Implement the sum of a harmonic series


>>> 1/1 + 1/2 + 1/3 + 1/4
2.083333333333333

>>> harmonics 4
2.083333333333333

>>> harmonics' 4
2.083333333333333
-}
harmonics :: (Fractional n) => Int -> n
harmonics n =
    foldr (+) 0 (take n $ genHarmonics 1)
  where
    genHarmonics :: (Fractional t) => t -> [t]
    genHarmonics n = 1 / n : genHarmonics (n + 1)

-- Alternative version (inspired from the book), not using explicit recursion
-- harmonics' :: (Fractional n, Enum n) => Int -> n
harmonics' :: (Fractional n, Enum n) => Int -> n
harmonics' n =
    sum (take n series)
  where
    series :: (Fractional n, Enum n) => [n]
    series = map (1.0 /) [1.0 ..]
