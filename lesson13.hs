module Lesson13 where

-- A type class example
class Describable a where
    describe :: a -> String

-- We can inherit behaviour via `deriving`
data IceCream
    = Chocolate
    | Vanilla
    deriving (Show, Eq, Ord)

{- |

Because we derived `Ord` (which itself requires `Eq`)
>>> Data.List.sort $ take 5 $ concat $ repeat [Chocolate, Vanilla]
[Chocolate,Chocolate,Chocolate,Vanilla,Vanilla]


Because we derived `Eq`
>>> Vanilla == Vanilla
True

>>> Vanilla == Chocolate
False

>>> Vanilla /= Chocolate
True

>>> min Chocolate Vanilla
Chocolate
>>> max Chocolate Vanilla
Vanilla
-}

{- | Q13.1

`Int` and `Word` implement the same type classes.

Int is for signed ints, and Word is for unsigned ints basically

  ghci> (maxBound :: Int) == (2 ^ 63 - 1)
  True
  ghci> (maxBound :: Word) == (2 ^ 64 - 1)
  True

ghci> (minBound::Word, minBound::Int)
(0,-9223372036854775808)

However, it's important to use `succ`/`pred` to prevent wrap around

  ghci> (\x -> x - 1) (0::Word)
  18446744073709551615
  ghci> pred (0::Word)
  *** Exception: Enum.pred{Word}: tried to take `pred' of minBound

So in summary, doing arithmetic operations on `Word` is wonky, one should use this type to represent bytes.
-}

{- | Q13.2

The difference between both behaviors is that one wraps around, and the other throws an exception

ghci> (+1) (maxBound :: Int)
-9223372036854775808
ghci> succ (maxBound :: Int)
*** Exception: Prelude.Enum.succ{Int}: tried to take `succ' of maxBound
-}

{- | Q13.3

Implement `cycleSucc`

>>> cycleSucc (maxBound :: Int)
-9223372036854775808

>>> cycleSucc (maxBound :: Word)
0
-}
cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc n =
    if n == maxBound
        then minBound
        else succ n
