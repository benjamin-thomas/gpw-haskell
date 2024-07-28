{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Functor law" #-}

import Data.Bifunctor (bimap)
import Data.Map (Map)
import Data.Map qualified as Map

type LatLong = (Double, Double)

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

-- q28.1
readDouble :: IO Double
readDouble = read <$> getLine

readLatLong :: IO LatLong
readLatLong = (,) <$> readDouble <*> readDouble

haversineIO :: IO Double
haversineIO = haversine <$> readLatLong <*> readLatLong

haversineIOb :: IO LatLong -> IO LatLong -> IO Double
haversineIOb ma mb = do
    a <- ma
    b <- mb
    return (haversine a b)

-- q28.2
haversineIOc :: IO LatLong -> IO LatLong -> IO Double
haversineIOc a b = haversine <$> a <*> b

-- q28.3

data Robot = Robot
    { rbtName :: String
    , rbtDescr :: String
    , rbtCost :: Double
    , rbtCount :: Int
    }
    deriving (Show)

robots :: Map Int Robot
robots =
    Map.fromList
        [ (1, Robot "R2D2" "R2-D2" 1.0 1)
        , (2, Robot "C3PO" "C-3PO" 2.0 2)
        , (3, Robot "BB8" "BB-8" 3.0 3)
        ]

cheapest :: Robot -> Robot -> Robot
cheapest a b = if rbtCost a < rbtCost b then a else b

lookupIO :: IO (Maybe Robot)
lookupIO = do
    id' :: Int <- read <$> getLine
    return $ Map.lookup id' robots

printCheapest :: Maybe Robot -> IO ()
printCheapest Nothing = putStrLn "No robot data!"
printCheapest (Just robot) = print robot

printCheapest' :: Maybe Double -> IO ()
printCheapest' Nothing = putStrLn "No cost data!"
printCheapest' (Just cost) = print cost

main :: IO ()
main = do
    putStrLn "Enter two robot IDs"
    ma <- lookupIO
    mb <- lookupIO
    let mCheapest = cheapest <$> ma <*> mb
    let mCheapest' = min <$> (rbtCost <$> ma) <*> (rbtCost <$> mb) -- <= book's solution
    printCheapest mCheapest
    printCheapest' mCheapest'
