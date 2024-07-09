{-# LANGUAGE LambdaCase #-}

module Lesson08 where

{- |

>>> drop 3 [1,2,3,4]
[4]
>>> drop 0 [1,2,3,4]
[1,2,3,4]

>>> drop' 3 [1,2,3,4]
[4]
>>> drop' 0 [1,2,3,4]
[1,2,3,4]

>>> drop' 3 []
[]
-}
drop' :: (Ord n, Num n) => n -> [a] -> [a]
drop' n [] = []
drop' n (x : xs)
    | n <= 0 = x : xs
    | n == 1 = xs
    | otherwise = drop' (n - 1) xs

{- |

>>> take' 3 [1,2,3,4]
[1,2,3]

>>> take' 0 [1,2,3,4]
[]

>>> take' 0 [1,2,3,4]
[]

>>> take' (-3) [1,2,3,4]
[]
-}
take' :: Int -> [a] -> [a]
take' _ [] = []
take' n (x : xs)
    | n <= 0 = []
    | otherwise = x : take' (n - 1) xs

{- |

>>> length' [1,2,3]
3
-}
length' :: [a] -> Integer
length' = \case
    [] -> 0
    (_ : xs) -> 1 + length' xs

{- |

>>> take' 6 $ cycle [1,2,3]
[1,2,3,1,2,3]
-}
cycle' :: [a] -> [a]
cycle' [] = error "cycle': empty list"
cycle' xs = xs ++ cycle' xs

cycle'' :: [a] -> [a]
cycle'' [] = error "cycle'': empty list"
cycle'' xs = xs' where xs' = xs ++ xs' -- <= from Prelude

-- Pathological recursion examples

ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) 1
ackermann m n = ackermann (m - 1) (ackermann m (n - 1))

collatz :: (Num a, Integral t) => t -> a
collatz 1 = 1
collatz n
    | even n = 1 + collatz (n `div` 2)
    | otherwise = 1 + collatz (n * 3 + 1)

{- | Q8.1 implement reverse


>>> reverse' "abc"
"cba"

>>> reverse' []
[]
-}
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

{- | Q8.2 implement fastFib

>> fastFib 1 1 0
0
>> fastFib 1 1 1
1
>> fastFib 1 1 2
1
>> fastFib 1 1 3
2
>> fastFib 1 1 4
3
>>> fastFib 1 1 5
5
>>> fastFib 1 1 10
55

>>> fastFib 1 1 15
610

>>> fastFib 1 1 1000
43466557686937456435688527675040625802564660517371780402481729089536555417949051890403879840079255169295922593080322634775209689623239873322471161642996440906533187938298969649928516003704476137795166849228875
-}
fastFib a _ 1 = a
fastFib a b counter = fastFib b (a + b) (counter - 1)
