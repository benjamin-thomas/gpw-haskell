module TimeSeries where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)

{-

ghcid -c 'ghci ./lesson20/*' -T ':!doctest ./lesson20/*.hs'

 -}

file1 :: [(Int, Double)]
file1 =
    [ (1, 200.1)
    , (2, 199.5)
    , (3, 199.4)
    , (4, 198.9)
    , (5, 199.0)
    , (6, 200.2)
    , (9, 200.3)
    , (10, 201.2)
    , (12, 202.9)
    ]

file2 :: [(Int, Double)]
file2 =
    [ (11, 201.6)
    , (12, 201.5)
    , (13, 201.5)
    , (14, 203.5)
    , (15, 204.9)
    , (16, 207.1)
    , (18, 210.5)
    , (20, 208.8)
    ]

file3 :: [(Int, Double)]
file3 =
    [ (10, 201.2)
    , (11, 201.6)
    , (12, 201.5)
    , (13, 201.5)
    , (14, 203.5)
    , (17, 210.5)
    , (24, 215.1)
    , (25, 218.7)
    ]

file4 :: [(Int, Double)]
file4 =
    [ (26, 219.8)
    , (27, 220.5)
    , (28, 223.8)
    , (29, 222.8)
    , (30, 223.8)
    , (31, 221.7)
    , (32, 222.3)
    , (33, 220.8)
    , (34, 219.4)
    , (35, 220.1)
    , (36, 220.6)
    ]

{-
We want to:

- stitch files into one (Semigroup or Monoid if needing to represent the identity element?)
- keep track of missing data (Nothing)
- perform analysis without having to worry about missing values

 -}

{- |

>>> minMax [2, 4, 1, 3]
(1,4)
-}
minMax :: (Ord a) => [a] -> (a, a)
minMax [] = error "minMax: empty list"
minMax xs =
    foldr
        (\x (currMin, currMax) -> (min currMin x, max currMax x))
        (head xs, head xs)
        (tail xs)

type Time = Int
data TimeSeries a = TimeSeries [Time] [Maybe a]

{- |

>>> createTimeSeries [1, 2, 4] ["one", "two", "four"]
1|"one"
2|"two"
3|NA
4|"four"
...
-}
createTimeSeries :: [Time] -> [a] -> TimeSeries a
createTimeSeries times values =
    TimeSeries completeTimes extendedValues
  where
    completeTimes =
        let (from, to) = minMax times in [from .. to]
    timeValueMap = Map.fromList $ zip times values
    extendedValues = map (`Map.lookup` timeValueMap) completeTimes

-- Let's imagine we have a file with a list of key/value pairs
fileToTimeSeries :: [(Time, a)] -> TimeSeries a
fileToTimeSeries pairs = createTimeSeries times values
  where
    (times, values) = unzip pairs

showPair :: (Show a) => Time -> Maybe a -> String
showPair time (Just value) = show time ++ "|" ++ show value ++ "\n"
showPair time Nothing = show time ++ "|" ++ "NA" ++ "\n"

instance (Show a) => Show (TimeSeries a) where
    show :: (Show a) => TimeSeries a -> String
    show (TimeSeries times values) =
        concatMap (uncurry showPair) (zip times values)

ts1 :: TimeSeries Double
ts1 = fileToTimeSeries file1

ts2 :: TimeSeries Double
ts2 = fileToTimeSeries file2

ts3 :: TimeSeries Double
ts3 = fileToTimeSeries file3

ts4 :: TimeSeries Double
ts4 = fileToTimeSeries file4

insertPair :: (Ord k) => Map k a -> (k, Maybe a) -> Map k a
insertPair dict (_, Nothing) = dict
insertPair dict (key, Just value) = Map.insert key value dict

{- |

>>> combineTimeSeries (createTimeSeries [1, 2, 4] ["one", "two", "four"]) (createTimeSeries [3, 6, 2] ["three", "six", "TWO"])
1|"one"
2|"TWO"
3|"three"
4|"four"
5|NA
6|"six"
...
-}
combineTimeSeries :: TimeSeries a -> TimeSeries a -> TimeSeries a
combineTimeSeries a (TimeSeries [] []) = a
combineTimeSeries (TimeSeries [] []) b = b
combineTimeSeries (TimeSeries kas vas) (TimeSeries kbs vbs) =
    TimeSeries completeTimes combinedValues
  where
    completeTimes =
        let (from, to) = minMax (kas <> kbs) in [from .. to]
    combinedValues =
        let dictA = foldl insertPair Map.empty (zip kas vas)
         in let dictAB = foldl insertPair dictA (zip kbs vbs)
             in map (`Map.lookup` dictAB) completeTimes

{-

> With `TimeSeries` an instance of `Semigroup`, you can now combine time series,
> automatically filling missing values and overwriting duplicate values.

 -}
instance Semigroup (TimeSeries a) where
    (<>) :: TimeSeries a -> TimeSeries a -> TimeSeries a
    (<>) = combineTimeSeries

{- |

>>> (createTimeSeries [1, 2, 4] ["one", "two", "four"]) <> (createTimeSeries [3, 6, 2] ["three", "six", "TWO"])
1|"one"
2|"TWO"
3|"three"
4|"four"
5|NA
6|"six"
...
-}

{-

> Being able to combine two or more `TimeSeries` with `<>` is useful.
> But given that you have four unique files to combine, it'd be even nicer
> if you could combine a list of them.

So we can define a `Monoid` instance.
 -}

instance Monoid (TimeSeries a) where
    mempty :: TimeSeries a
    mempty = TimeSeries [] []

{- |

>>> mconcat [(createTimeSeries [1, 2, 4] ["one", "two", "four"]), (createTimeSeries [3, 6, 2] ["three", "six", "TWO"]), (createTimeSeries [7, 8, 9] ["seven", "eight", "nine"])]
1|"one"
2|"TWO"
3|"three"
4|"four"
5|NA
6|"six"
7|"seven"
8|"eight"
9|"nine"
...
-}
tsAll :: TimeSeries Double
tsAll = mconcat [ts1, ts2, ts3, ts4]

{- | We start by the simplest analysis: average of the values

>>> mean [1, 2, 3, 4, 5]
3.0

>>> mean [1.0, 2.5, 2.5, 4.0, 5.0]
3.0
-}
mean :: (Real a) => [a] -> Double
mean xs = total / count
  where
    total = (realToFrac . sum) xs
    count = (realToFrac . length) xs

{- | Then you can average a `TimeSeries`
>>> meanTimeSeries tsAll
Just 210.5966666666667
-}
meanTimeSeries :: (Real a) => TimeSeries a -> Maybe Double
meanTimeSeries (TimeSeries _ []) = Nothing
meanTimeSeries (TimeSeries _times values) =
    if all (== Nothing) values
        then Nothing
        else
            Just avg
  where
    -- justVals = filter isJust values
    -- cleanVals = map fromJust justVals
    -- avg = mean cleanVals
    avg = mean $ catMaybes values

type CompareFunc a = a -> a -> a
type TimeSeriesCompareFunc a = (Int, Maybe a) -> (Int, Maybe a) -> (Int, Maybe a)

{- |

>>> makeTimeSeriesCompare max (3, Just 200) (4, Just 10)
(3,Just 200)

>>> makeTimeSeriesCompare max (3, Nothing) (4, Just 10)
(4,Just 10)

>>> makeTimeSeriesCompare max (3, Just 200) (4, Nothing)
(3,Just 200)

>>> makeTimeSeriesCompare max (3, Nothing) (4, Nothing)
(3,Nothing)
-}

-- makeTimeSeriesCompare :: (Eq a) => CompareFunc a -> TimeSeriesCompareFunc a
-- makeTimeSeriesCompare compare' = f
--   where
--     f a (_, Nothing) = a
--     f (_, Nothing) b = b
--     f (ka, Just va) (kb, Just vb) =
--         if compare' va vb == va
--             then
--                 (ka, Just va)
--             else
--                 (kb, Just vb)
-- makeTimeSeriesCompare :: (Eq a) => CompareFunc a -> TimeSeriesCompareFunc a
-- makeTimeSeriesCompare compare' a (_, Nothing) = a
-- makeTimeSeriesCompare compare' (_, Nothing) b = b
-- makeTimeSeriesCompare compare' (ka, Just va) (kb, Just vb) =
--     if compare' va vb == va
--         then
--             (ka, Just va)
--         else
--             (kb, Just vb)
makeTimeSeriesCompare :: (Eq a) => CompareFunc a -> TimeSeriesCompareFunc a
makeTimeSeriesCompare compare' x y = case (x, y) of
    (a, (_, Nothing)) -> a
    ((_, Nothing), b) -> b
    ((ka, Just va), (kb, Just vb)) ->
        if compare' va vb == va
            then
                (ka, Just va)
            else
                (kb, Just vb)

compareTimeSeries :: (Eq a) => CompareFunc a -> TimeSeries a -> Maybe (Int, Maybe a)
compareTimeSeries _ (TimeSeries [] []) = Nothing
compareTimeSeries compare' (TimeSeries times values) =
    if all (== Nothing) values
        then
            Nothing
        else Just best
  where
    pairs = zip times values
    best = foldl (makeTimeSeriesCompare compare') (0, Nothing) pairs

{- |

>>> minTimeSeries tsAll
Just (4,Just 198.9)

>>> maxTimeSeries ts1
Just (12,Just 202.9)
-}
minTimeSeries :: (Ord a) => TimeSeries a -> Maybe (Int, Maybe a)
minTimeSeries = compareTimeSeries min

maxTimeSeries :: (Ord a) => TimeSeries a -> Maybe (Int, Maybe a)
maxTimeSeries = compareTimeSeries max

diffPair :: (Num a) => Maybe a -> Maybe a -> Maybe a
diffPair Nothing _ = Nothing
diffPair _ Nothing = Nothing
diffPair (Just a) (Just b) = Just (a - b)

{- | Sales have grown about 0.6 each month

>>> meanTimeSeries $ diffTimeSeries tsAll
Just 0.6076923076923071
-}
diffTimeSeries :: (Num a) => TimeSeries a -> TimeSeries a
diffTimeSeries (TimeSeries [] []) = TimeSeries [] []
diffTimeSeries (TimeSeries times values) =
    TimeSeries times (Nothing : diffValues)
  where
    shiftValues = tail values
    diffValues = zipWith diffPair shiftValues values

meanMaybe :: (Real a) => [Maybe a] -> Maybe Double
meanMaybe vals =
    if Nothing `elem` vals
        then
            Nothing
        else
            Just avg
  where
    avg = mean $ catMaybes vals

movingAvg :: (Real a) => [Maybe a] -> Int -> [Maybe Double]
movingAvg [] _ = []
movingAvg vals n =
    if length nextVals == n
        then
            meanMaybe nextVals : movingAvg restVals n
        else []
  where
    nextVals = take n vals
    restVals = tail vals

movingAverageTimeSeries :: (Real a) => TimeSeries a -> Int -> TimeSeries Double
movingAverageTimeSeries (TimeSeries [] []) _ = TimeSeries [] []
movingAverageTimeSeries (TimeSeries times values) n =
    TimeSeries times smoothedValues
  where
    ma = movingAvg values n
    nothings = replicate (n `div` 2) Nothing
    smoothedValues = mconcat [nothings, ma, nothings]
