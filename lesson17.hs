{-# HLINT ignore "Use last" #-}
{-# HLINT ignore "Use minimum" #-}
{-# HLINT ignore "Use all" #-}
{-# HLINT ignore "Use any" #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Lesson17 where

import Data.List (sort)

-- Semigroups and Monoids

-- Combining functions

{- |

>>> last' [1, 2, 3]
3

>>> minimum' [2, 1, 3]
1

>>> maximum' [2, 3, 1]
3
-}
last' :: [a] -> a
last' = head . reverse

minimum' :: (Ord a) => [a] -> a
minimum' = head . sort

maximum' :: (Ord a) => [a] -> a
maximum' = last' . sort

{- |

>>> all' (> 0) [1, 2, 3]
True
>>> all' (> 0) []
True

>>> all' odd [1, 2, 3]
False

>>> all' odd [1, 3, 5]
True
-}
all' :: (a -> Bool) -> [a] -> Bool
-- all' test = foldr (&&) True . (map test)
all' test = and . map test

{- |

>>> any' (> 0) [1, 2, 3]
True

>>> any (> 0) []
False

>>> any odd [1, 2, 3]
True

>>> any odd [2, 4, 6]
False
-}
any' :: (a -> Bool) -> [a] -> Bool
-- any' test = foldr (||) False . (map test)
any' test = or . map test

-- Combining things with (<>)

instance Semigroup Integer where
    (<>) :: Integer -> Integer -> Integer
    a <> b = a + b

{- |

>>> (1::Integer) + (2::Integer)
3

>>> (1::Integer) <> (2::Integer)
3
-}
data Color
    = Red
    | Green
    | Blue
    | Yellow
    | Purple
    | Orange
    | Brown
    | Transparent -- Q17.1
    deriving (Show, Eq)

instance Semigroup Color where
    (<>) :: Color -> Color -> Color
    (<>) Transparent b = b
    (<>) a Transparent = a
    (<>) Red Blue = Purple
    (<>) Blue Red = Purple
    (<>) Yellow Blue = Green
    (<>) Blue Yellow = Green
    (<>) Red Yellow = Orange
    (<>) Yellow Red = Orange
    -- (<>) a b = if a == b then a else Brown
    (<>) a b
        | a == b = a
        | all (`elem` [Red, Blue, Purple]) [a, b] = Purple
        | all (`elem` [Blue, Yellow, Green]) [a, b] = Green
        | all (`elem` [Red, Yellow, Orange]) [a, b] = Orange
        | otherwise = Brown

instance Monoid Color where -- Q17.1
    mempty = Transparent

{- | Q17.1

>>> mconcat [Red, Transparent, Red]
Red

>>> mconcat [Red, Transparent, Red, Transparent, Blue, Transparent]
Purple
-}

{- |

>>> Red <> Blue
Purple


We have a problem though: we broke the associativity "law"
>> (Green <> Blue) <> Yellow
Brown

>> Green <> (Blue <> Yellow)
Green

Fixed!
>>> (Green <> Blue) <> Yellow
Green

>>> Green <> (Blue <> Yellow)
Green
-}

{-
The only major difference between Semigroups and Monoid is that Monoid requires an
identity element, i.e as below

x  <> id = x

id <> x  = x

So for Integer, the identity element is 0.

Having an identity element enables one to use fold to combine a list of things.

 -}

{- | Due to history (Monoid was defined before Semigroups in Haskell), we get quirky names and declarations for Monoid

So Monoid implements 3 functions:
    - mempty
    - mappend
    - mconcat (can be deduced from mempty + mappend)

mconcat can be deduced because its default definition is:

  mconcat = foldr mappend mempty

---

Also, some kind of duplication:

>>> [1,2,3] ++ []
[1,2,3]

Same as:
>>> [1,2,3] <> []
[1,2,3]

Same as:
>>> [1,2,3] `mappend` mempty
[1,2,3]
-}

{- |

So List is a Monoid (see: ghci> :info [])

And so:
>>> "Does" ++ " this" ++ " make" ++ " sens?"
"Does this make sens?"

Same as:
>>> "Does" <> " this" <> " make" <> " sens?"
"Does this make sens?"

Same as:
>>> mconcat ["Does", " this", " make", " sens?"]
"Does this make sens?"

(NOTE: in PureScript, `mconcat` is called `fold`)

>>> mconcat $ Data.List.intersperse " " ["Hello", "how", "are", "you?"]
"Hello how are you?"

(
    In PureScript
    > import Data.Array as Array
    > Array.fold $ Array.intersperse " " ["Does", "this", "make", "sens?"]
    > import Data.Foldable as Foldable
    > Foldable.fold $ Array.intersperse " " ["Does", "this", "make", "sens?"]

)
-}

{- | Monoid has 4 "laws"


1) mappend mempty x = x
>>> mappend mempty [1,2,3]
[1,2,3]

2) mappend x mempty = x
>>> mappend [1,2,3] mempty
[1,2,3]

3) mappend x (mappend y z) = mappend (mappend x y) z
This associativity law comes from Semigroups
>>> mappend [1,2,3] (mappend [4,5,6] [7,8,9])
[1,2,3,4,5,6,7,8,9]

>>> mappend (mappend [1,2,3] [4,5,6]) [7,8,9]
[1,2,3,4,5,6,7,8,9]

4) mconcat = foldr mappend mempty
>>> mconcat [[1,2,3],[4,5,6],[7,8,9]]
[1,2,3,4,5,6,7,8,9]
>>> foldr mappend mempty [[1,2,3],[4,5,6],[7,8,9]]
[1,2,3,4,5,6,7,8,9]
-}
