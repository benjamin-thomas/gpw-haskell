{-# OPTIONS_GHC -Wno-type-defaults #-}

import Data.Bifunctor (bimap)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isJust)
import GHC.IO.Handle.FD (stdout)
import System.IO (hFlush)

fullName :: Maybe String -> Maybe String -> Maybe String
fullName ma mb =
  case (ma, mb) of
    (Just a, Just b) -> Just (a <> " " <> b)
    _ -> Nothing

type LatLong = (Double, Double)

locationDB :: Map String LatLong
locationDB =
  Map.fromList
    [ ("Arkham", (42.6054, -70.7829))
    , ("Innsmouth", (42.8250, -70.8150))
    , ("Carcosa", (29.9714, -90.7694))
    , ("New York", (40.7776, -73.9691))
    ]

toRadians :: Double -> Double
toRadians degrees = degrees * pi / 180

biRads :: LatLong -> LatLong
biRads = bimap toRadians toRadians

haversine :: LatLong -> LatLong -> Double
haversine coords1 coords2 = earthRadius * c
 where
  (rLat1, rLong1) = biRads coords1
  (rLat2, rLong2) = biRads coords2
  dLat = rLat2 - rLat1
  dLong = rLong2 - rLong1
  a = sin (dLat / 2) ^ 2 + cos rLat1 * cos rLat2 * sin (dLong / 2) ^ 2
  c = 2 * atan2 (sqrt a) (sqrt (1 - a))
  earthRadius = 3961

printDistance :: Maybe Double -> IO ()
printDistance Nothing = putStrLn "Error, invalid city entered"
printDistance (Just distance) = putStrLn (show distance <> " miles")

{-

As with `Functor`, we can implement a "naive" wrapper for our specialized
function, but that'd generate a lot of boilerplate.

 -}
haversineMaybe :: Maybe LatLong -> Maybe LatLong -> Maybe Double
haversineMaybe Nothing _ = Nothing
haversineMaybe _ Nothing = Nothing
haversineMaybe (Just coords1) (Just coords2) = Just (haversine coords1 coords2)

-- Quick check 28.1
addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe (Just a) (Just b) = Just (a + b)
addMaybe _ _ = Nothing

-- Quick check 28.2
distanceFromNY :: String -> Maybe Double
distanceFromNY x =
  fromNY <$> Map.lookup x locationDB
 where
  fromNY :: LatLong -> Double
  fromNY = haversine (40.7776, -73.9691)

{-

Whereas fmap/<$> allows us to apply a function to a value in a context,
app/<*> allows us to apply a function in a context, to a value in a context.

>>> let inc = (1+)

>>> inc <$> Just 1
Just 2

>>> let maybeInc = (+) <$> Just 1
>>> maybeInc <*> Just 2
Just 3

>>> (+) <$> Just 1 <*> Just 2
Just 3

>>> (+) <$> Just 1 <*> Nothing
Nothing

>>> (+) <$> Nothing <*> Just 2
Nothing

>>> (\a b c -> a + b + c) <$> Just 1 <*> Just 2 <*> Just 3
Just 6

>>> (\a b c -> a + b + c) <$> Nothing <*> Just 2 <*> Just 3
Nothing

>>> (\a b c -> a + b + c) <$> Just 1 <*> Nothing <*> Just 3
Nothing

>>> (\a b c -> a + b + c) <$> Just 1 <*> Just 2 <*> Nothing
Nothing

We can now use any binary function in a context

>>> (++) <$> Just "cats" <*> Just " and dogs"
Just "cats and dogs"

>>> (<>) <$> Just "cats" <*> Just " and dogs"
Just "cats and dogs"

>>> (<>) <$> Nothing <*> Just " and dogs"
Nothing

>>> (<>) <$> Just "cats" <*> Nothing
Nothing

 -}

-- q28.3
{-
>>> (*) <$> Just 10 <*> Just 5
Just 50

>>> div <$> Just 10 <*> Just 5
Just 2

>>> mod <$> Just 10 <*> Just 5
Just 0

 -}

-- Line won't flush when compiled (but ok when ran via ghci or runghc)
puts :: String -> IO ()
puts x = do
  putStr x
  hFlush stdout

main :: IO ()
main = do
  puts "Enter the starting city name: "
  startingInput <- getLine
  let startingCity = Map.lookup startingInput locationDB
  puts "Enter the ending city name: "
  endingInput <- getLine
  let endingCity = Map.lookup endingInput locationDB
  case haversine <$> startingCity <*> endingCity of
    Nothing -> do
      putStrLn "Sorry, I don't have that data!"
      putStrLn
        ( mconcat
            [ "Found cities are: "
            , show
                ( map
                    fst
                    ( filter
                        (isJust . snd)
                        [ (startingInput, startingCity)
                        , (endingInput, endingCity)
                        ]
                    )
                )
            ]
        )
    Just dist ->
      putStrLn $
        mconcat
          [ "Distance between "
          , startingInput
          , " and "
          , endingInput
          , " is: "
          , show dist
          ]
