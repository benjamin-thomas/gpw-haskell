{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}

import Control.Applicative (Alternative)
import Control.Monad (guard)
import Data.Bifunctor (Bifunctor (first), bimap)
import Data.Char (toUpper)
import Data.Functor ((<&>))
import Data.List (uncons)

{-

The main use of the List Monad is to generate lists

>>> powersOfTwoMap 8
[2,4,8,16,32,64,128,256]

>>> powersOfTwo 8
[2,4,8,16,32,64,128,256]

>>> powersOfTwoComp 8
[2,4,8,16,32,64,128,256]
 -}
powersOfTwoMap :: Int -> [Int]
powersOfTwoMap n = map (\val -> 2 ^ val) [1 .. n]

-- The advantage of this version is that we can think in terms of handling single values
powersOfTwo :: Int -> [Int]
powersOfTwo n = do
    value <- [1 .. n]
    return $ 2 ^ value

-- Using list comprehension syntax
powersOfTwoComp :: Int -> [Int]
powersOfTwoComp n = [2 ^ val | val <- [1 .. n]]

{-

>>> powerOfTwoAndThree 8
[(2,3),(4,9),(8,27),(16,81),(32,243),(64,729),(128,2187),(256,6561)]

>>> powerOfTwoAndThreeComp 8
[(2,3),(4,9),(8,27),(16,81),(32,243),(64,729),(128,2187),(256,6561)]

 -}
powerOfTwoAndThree :: Int -> [(Int, Int)]
powerOfTwoAndThree n = do
    val <- [1 .. n]
    return (2 ^ val, 3 ^ val)

powerOfTwoAndThreeComp :: Int -> [(Int, Int)]
powerOfTwoAndThreeComp n =
    [ (2 ^ val, 3 ^ val) | val <- [1 .. n]
    ]

{-

>>> allEvenOdds 4
[(2,1),(2,3),(4,1),(4,3)]

>>> allEvenOdds 5
[(2,1),(2,3),(2,5),(4,1),(4,3),(4,5)]

>>> allEvenOdds 6
[(2,1),(2,3),(2,5),(4,1),(4,3),(4,5),(6,1),(6,3),(6,5)]

>>> allEvenOddsComp 6
[(2,1),(2,3),(2,5),(4,1),(4,3),(4,5),(6,1),(6,3),(6,5)]

 -}
allEvenOdds :: Int -> [(Int, Int)]
allEvenOdds n = do
    evenValue <- [2, 4 .. n] -- [2,4,6,8..n]
    oddValue <- [1, 3 .. n] -- [1,3,5,7..n]
    return (evenValue, oddValue)

allEvenOddsComp :: Int -> [(Int, Int)]
allEvenOddsComp n =
    [ (evenVal, oddVal)
    | evenVal <- [2, 4 .. n]
    , oddVal <- [1, 3 .. n]
    ]

-- Quick check 32.1
-- >>> squares 6
-- [1,4,9,16,25,36]
squares n = do
    x <- [1 .. n]
    return $ x * x

{-

When working with certain monads (such as List), we can use `guard` to filter
items.

`guard` comes from the `Alternative` type class.
`Alternative` is a subclass of `Applicative`.
Both `Maybe` and `List` are subclasses of `Alternative`.

guard :: Alternative f => Bool -> f ()

The underlying function (implemented by `Alternative`) is `empty`.

    ghci> Control.Applicative.empty :: Maybe Int
    Nothing
    ghci> Control.Applicative.empty :: [Int]
    []

>>> evensGuard 10
[2,4,6,8,10]

>>> evensGuard 11
[2,4,6,8,10]

>>> evensGuard 12
[2,4,6,8,10,12]

>>> evensGuardComp 12
[2,4,6,8,10,12]
 -}

evensGuard :: Int -> [Int]
evensGuard n = do
    val <- [1 .. n]
    guard (even val)
    return val

evensGuardComp :: Int -> [Int]
evensGuardComp n =
    [ val | val <- [1 .. n], even val
    ]

{-

Quick check 32.2

>>> filter even [1..10]
[2,4,6,8,10]

>>> filter' even [1..10]
[2,4,6,8,10]

 -}
-- filter' :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
filter' :: (a -> Bool) -> [a] -> [a]
filter' test xs = do
    x <- xs
    guard (test x)
    return x

{-

>>> take 4 evenSquares
[0,4,16,36]

>>> take 4 evenSquaresComp
[0,4,16,36]

 -}
evenSquares :: [Int]
evenSquares = do
    n <- [0 .. 9]
    let nSquared = n ^ 2
    guard (even nSquared)
    return nSquared

evenSquaresComp :: [Int]
evenSquaresComp =
    [ nSquared
    | n <- [0 .. 9]
    , let nSquared = n ^ 2
    , even nSquared
    ]

{-

>>> capitalize ""
Nothing

>>> capitalize "abc"
Just "Abc"

>>> capitalize' ""
Nothing

>>> capitalize' "abc"
Just "Abc"

>>> capitalize'' ""
Nothing

>>> capitalize'' "abc"
Just "Abc"

 -}
capitalize :: String -> Maybe String
capitalize str =
    (\(x, xs) -> toUpper x : xs) <$> uncons str

capitalize' :: String -> Maybe String
capitalize' str = do
    (x, xs) <- uncons str
    return (toUpper x : xs)

capitalize'' :: String -> Maybe String
capitalize'' str =
    uncons str <&> (\(x, xs) -> toUpper x : xs)

{-

Quick check 32.3

>>> people
[Just "Mr Brown",Just "Mr Blue",Just "Mr Pink",Just "Mr Orange"]

mapMaybe is like filterMap in Elm
>>> Data.Maybe.mapMaybe id people
["Mr Brown","Mr Blue","Mr Pink","Mr Orange"]

>>> Data.Maybe.catMaybes people
["Mr Brown","Mr Blue","Mr Pink","Mr Orange"]

 -}

people :: [Maybe String]
people =
    [ person
    | color <- ["brown", "blue", "pink", "orange"]
    , let capitalized = capitalize color
    , let person = (\x -> mconcat ["Mr", " ", x]) <$> capitalized
    ]
