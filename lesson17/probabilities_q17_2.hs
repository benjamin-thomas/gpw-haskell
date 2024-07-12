module Probabilities_Q17_2 where

import Text.Printf (PrintfType, printf)

newtype Events = Events [String] deriving (Show)
newtype Probs = Probs [Double] deriving (Show)

data PTable = PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable events (Probs props) = PTable events (Probs normalizedProbs)
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
    show (PTable (Events events) (Probs probs)) = mconcat kvs
      where
        kvs :: [String]
        kvs = zipWith (showKV widest) events probs

        widest :: Int
        widest = maximum $ map length events

{- | Flipping a coin has 50/50 probability of being heads or tails

>>> createPTable (Events ["heads", "tails"]) (Probs [0.5, 0.5])
heads | 0.5
tails | 0.5
...
-}

{- | Different ways to create Cartesian products

>>> cartCombine (,) [1,2,3] [3,4,5]
[(1,3),(1,4),(1,5),(2,3),(2,4),(2,5),(3,3),(3,4),(3,5)]
-}
cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine f xs ys =
    [f x y | x <- xs, y <- ys]

{- |

>>> Events ["AAA", "BBB"] <> Events ["CCC", "DDD"]
Events ["AAA-CCC","AAA-DDD","BBB-CCC","BBB-DDD"]

>>> Events ["AAA", "BBB"] <> Events [] <> Events ["CCC", "DDD"]
Events ["AAA-CCC","AAA-DDD","BBB-CCC","BBB-DDD"]

>>> mconcat [Events ["AAA", "BBB"], Events [], Events ["CCC", "DDD"]]
Events ["AAA-CCC","AAA-DDD","BBB-CCC","BBB-DDD"]
-}
instance Semigroup Events where
    (<>) :: Events -> Events -> Events
    (<>) (Events []) b = b
    (<>) a (Events []) = a
    (<>) (Events as) (Events bs) = Events $ cartCombine f as bs
      where
        f x y = mconcat [x, "-", y]

instance Monoid Events where
    mempty :: Events
    mempty = Events []

    mappend = (<>)

{- |
To combine probabilities, we need to multiply them
>>> Probs [2.0, 3.0] <> Probs [4.0, 5.0]
Probs [8.0,10.0,12.0,15.0]

>>> mconcat [Probs [2.0, 3.0], Probs [1]]
Probs [2.0,3.0]
-}
instance Semigroup Probs where
    (<>) :: Probs -> Probs -> Probs
    (<>) a (Probs []) = a
    (<>) (Probs []) b = b
    (<>) (Probs xs) (Probs ys) = Probs $ cartCombine (*) xs ys

instance Monoid Probs where
    mempty :: Probs
    mempty = Probs []

-- Now we can make PTable an instance of Semigroup since we can combine all of its components

instance Semigroup PTable where
    (<>) :: PTable -> PTable -> PTable
    (<>) a (PTable (Events []) (Probs [])) = a
    (<>) (PTable (Events []) (Probs [])) b = b
    (<>) (PTable e1 p1) (PTable e2 p2) =
        createPTable (e1 <> e2) (p1 <> p2)

-- And now we can implement Monoid. We obtain `mconcat` "for free"
instance Monoid PTable where
    mempty :: PTable
    mempty = PTable (Events []) (Probs [])
    mappend :: PTable -> PTable -> PTable
    mappend = (<>)

coin :: PTable
coin =
    createPTable
        (Events ["heads", "tails"])
        (Probs [0.5, 0.5])

spinner :: PTable
spinner =
    createPTable
        (Events ["red", "blue", "green"])
        (Probs [0.1, 0.2, 0.7])

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
