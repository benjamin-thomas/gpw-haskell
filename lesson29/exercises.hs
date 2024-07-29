{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

{- q29.1
To prove that `Applicative` is strictly more powerful than `Functor`, write a
universal version of `fmap` called `allFmap`, that defines `fmap` for all
members of the `Applicative` type class.
-}

allFmap :: (Applicative f) => (a -> b) -> f a -> f b
allFmap func item = pure func <*> item

{- |

>>> allFmap (+1) [1,2,3]
[2,3,4]

>>> allFmap (+1) (Just 5)
Just 6

>>> allFmap (+5) Nothing
Nothing
-}

{- q29.2
-}

example :: Maybe Int
example = pure $ (*) ((+) 2 4) 6

{-  q29.3 (I had a peek at the solution for that one)

Use nondeterministic computing with lists to determine how much beer you need
to purchase.

1) You bought beer last night, but you don't remember if it was a 6-pack or a
  12-pack.

2) You and your friend each had 2 beers last night

3) You're having either 2 or 3 friends come over

4) You expect the average person will drink 3 to 4 beers

 -}
startingBeerRange :: [Int] -- 1)
startingBeerRange = [6, 12]

-- >>> remainingBeerRange
-- [2,8]
remainingBeerRange :: [Int] -- 2)
remainingBeerRange =
    -- pure (\x -> x - 4) <*> startingBeerRange
    pure (subtract 4) <*> startingBeerRange

guestsRange :: [Int] -- 3)
guestsRange = [2, 3]

-- >>> totalPeopleRange
-- [4,5]
totalPeopleRange :: [Int] -- (my friend + myself) + the guests
totalPeopleRange = (+ 2) <$> guestsRange

beersPerGuestRange :: [Int] -- 4)
beersPerGuestRange =
    [3, 4]

-- >>> totalBeersNeededRange
-- [12,15,16,20]
-- >>> maximum totalBeersNeededRange
-- 20
totalBeersNeededRange :: [Int]
totalBeersNeededRange =
    (*) <$> beersPerGuestRange <*> totalPeopleRange

-- >>> (remainingBeerRange, totalBeersNeededRange)
-- ([2,8],[12,15,16,20])
-- >>> beersToPurchaseRange
-- [10,13,14,18,4,7,8,12]
beersToPurchaseRange :: [Int]
beersToPurchaseRange =
    subtract <$> remainingBeerRange <*> totalBeersNeededRange

{- NOTE: this would have also worked

ghci> import Data.List -- sort

ghci> subtract <$> remainingBeerRange <*> totalBeersNeededRange
[10,13,14,18,4,7,8,12]
ghci> (-) <$> totalBeersNeededRange  <*> remainingBeerRange
[10,4,13,7,14,8,18,12]
ghci> sort $ subtract <$> remainingBeerRange <*> totalBeersNeededRange
[4,7,8,10,12,13,14,18]
ghci> sort $ (-) <$> totalBeersNeededRange  <*> remainingBeerRange
[4,7,8,10,12,13,14,18]

 -}

-- >>> beersToPurchase
-- 18

{-

ghci> (3+2)*4 - (6-4)
18

Assume there remains only 2 beers (at worst)              <== step 1 and 2

ghci> (min 6 12) - (2 * 2)
2

Max people will drink max 4 beers, so 20 beers total max  <== step 3 and 4
ghci> ((max 3 2) + 2) * (max 3 4)
20

20-2=18
 -}

beersToPurchase :: Int
beersToPurchase = maximum beersToPurchaseRange
