module Probabilities where

import Control.Applicative (liftA2)
import Control.Monad (liftM2)
import Text.Printf (PrintfType, printf)

type Events = [String]
type Probs = [Double]

data PTable = PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable events props = PTable events normalizedProbs
  where
    normalizedProbs = map (/ sum props) props

{- |

>>> ">>" ++ padR 0 "ABC" ++ "<<"
">>ABC<<"


>>> ">>" ++ padR 4 "ABC" ++ "<<"
">>ABC <<"
-}
padR :: (PrintfType r, Show a) => a -> r
padR widest = printf ("%-" ++ show widest ++ "s")

showKV :: Int -> String -> Double -> String
showKV widest evt prob =
    -- Using printf exclusively would probably be more appropriate here, but
    -- this is for demonstration's purposes
    mconcat
        [ padR widest evt
        , " | "
        , show prob
        , "\n"
        ]

instance Show PTable where
    show :: PTable -> String
    show (PTable events probs) = mconcat kvs
      where
        kvs :: [String]
        kvs = zipWith (showKV widest) events probs

        widest :: Int
        widest = maximum $ map length events

{- | Flipping a coin has 50/50 probability of being heads or tails

>>> createPTable ["heads", "tails"] [0.5, 0.5]
heads | 0.5
tails | 0.5
...
-}

{- |
Who gets front seat?
>>> createPTable ["Dorian", "Ewan", "Pénélope"] [0.5, 0.5, 0.5]
Dorian   | 0.3333333333333333
Ewan     | 0.3333333333333333
Pénélope | 0.3333333333333333
...
-}

{- | Different ways to create Cartesian products

>>> cartCombine (,) [1,2,3] [3,4,5]
[(1,3),(1,4),(1,5),(2,3),(2,4),(2,5),(3,3),(3,4),(3,5)]

>>> cartCombine2 (,) [1,2,3] [3,4,5]
[(1,3),(1,4),(1,5),(2,3),(2,4),(2,5),(3,3),(3,4),(3,5)]

>>> cartCombine3 (,) [1,2,3] [3,4,5]
[(1,3),(1,4),(1,5),(2,3),(2,4),(2,5),(3,3),(3,4),(3,5)]

>>> cartCombine4 (,) [1,2,3] [3,4,5]
[(1,3),(1,4),(1,5),(2,3),(2,4),(2,5),(3,3),(3,4),(3,5)]

>>> cartCombine5 (,) [1,2,3] [3,4,5]
[(1,3),(1,4),(1,5),(2,3),(2,4),(2,5),(3,3),(3,4),(3,5)]

>>> cartCombine6 (,) [1,2,3] [3,4,5]
[(1,3),(1,4),(1,5),(2,3),(2,4),(2,5),(3,3),(3,4),(3,5)]

>>> cartCombine7 (,) [1,2,3] [3,4,5]
[(1,3),(1,4),(1,5),(2,3),(2,4),(2,5),(3,3),(3,4),(3,5)]

>>> cartCombine8 (,) [1,2,3] [3,4,5]
[(1,3),(1,4),(1,5),(2,3),(2,4),(2,5),(3,3),(3,4),(3,5)]

>>> cartCombine9 (,) [1,2,3] [3,4,5]
[(1,3),(1,4),(1,5),(2,3),(2,4),(2,5),(3,3),(3,4),(3,5)]
-}
cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine f xs ys =
    [f x y | x <- xs, y <- ys]

-- Same but without list comprehension (the book's version, cleaned up)
cartCombine2 :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine2 f xs ys =
    zipWith f xs' ys'
  where
    xs' = mconcat $ map (replicate $ length ys) xs -- ==> [1,1,1,2,2,2,3,3,3]
    ys' = cycle ys --                                 ==> [3,4,5,3,4,5,3,4,5]

-- OCaml style
cartCombine3 :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine3 f xs ys = concatMap (\x -> map (f x) ys) xs

cartCombine4 :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine4 f xs ys = foldr (\x xAcc -> foldr (\y yAcc -> f x y : yAcc) xAcc ys) [] xs

cartCombine5 :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine5 = liftM2

cartCombine6 :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine6 = liftA2

cartCombine7 :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine7 f xs ys = f <$> xs <*> ys

cartCombine8 :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine8 f xs ys = do
    x <- xs
    f x <$> ys

{- FOURMOLU_DISABLE -}
cartCombine9 :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine9 f xs ys =
    xs >>= \x ->
    ys >>= \y ->
    pure $ f x y
{- FOURMOLU_ENABLE -}

{- |

>>> combineEvents ["AAA", "BBB"] ["CCC", "DDD"]
["AAA-CCC","AAA-DDD","BBB-CCC","BBB-DDD"]
-}
combineEvents :: Events -> Events -> Events
combineEvents = cartCombine f
  where
    f x y = mconcat [x, "-", y]

{- |
To combine probabilities, we need to multiply them
>>> combineProbs [2.0, 3.0] [4.0, 5.0]
[8.0,10.0,12.0,15.0]
-}
combineProbs :: Probs -> Probs -> Probs
combineProbs = cartCombine (*)

-- Now we can make PTable an instance of Semigroup since we can combine all of its components

instance Semigroup PTable where
    (<>) :: PTable -> PTable -> PTable
    (<>) a (PTable [] []) = a
    (<>) (PTable [] []) b = b
    (<>) (PTable e1 p1) (PTable e2 p2) =
        createPTable (combineEvents e1 e2) (combineProbs p1 p2)

-- And now we can implement Monoid. We obtain `mconcat` "for free"
instance Monoid PTable where
    mempty = PTable [] []
    mappend = (<>)

coin :: PTable
coin = createPTable ["heads", "tails"] [0.5, 0.5]

spinner :: PTable
spinner = createPTable ["red", "blue", "green"] [0.1, 0.2, 0.7]

{- | To obtain the probability of getting `tails` on the coin and `blue` on the spinner, we use (<>)

>>> coin <> spinner
heads-red   | 5.0e-2
heads-blue  | 0.1
heads-green | 0.35
tails-red   | 5.0e-2
tails-blue  | 0.1
tails-green | 0.35
...

For instance, we can see that there's a 10% probability (0.1) of flipping tails and spinning blue.
-}

{- |



To obtain the probability of flipping heads 3 times in a row, we use `mconcat`:

>>> mconcat [coin, coin, coin]
heads-heads-heads | 0.125
heads-heads-tails | 0.125
heads-tails-heads | 0.125
heads-tails-tails | 0.125
tails-heads-heads | 0.125
tails-heads-tails | 0.125
tails-tails-heads | 0.125
tails-tails-tails | 0.125
...

In this case, each outcome has the same probability: 12.5%.
-}
