{-# LANGUAGE LambdaCase #-}

module Lesson19 where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isJust, isNothing)

data Organ
    = Heart
    | Brain
    | Kidney
    | Spleen
    deriving (Show, Eq)

organPairs :: [(Int, Organ)]
organPairs = zip ids organs
  where
    organs :: [Organ]
    organs =
        [ Heart
        , Heart
        , Brain
        , Spleen
        , Spleen
        , Kidney
        ]

    ids :: [Int]
    ids = [2, 7, 13, 14, 21, 24]

organCatalog :: Map Int Organ
organCatalog = Map.fromList organPairs

possibleDrawers :: [Int]
possibleDrawers = [1 .. 50]

{- |

>>> take 7 $ getDrawerContents possibleDrawers organCatalog
[Nothing,Just Heart,Nothing,Nothing,Nothing,Nothing,Just Heart]
-}
getDrawerContents :: [Int] -> Map Int Organ -> [Maybe Organ]
getDrawerContents ids catalog =
    map getValue ids
  where
    getValue :: Int -> Maybe Organ
    getValue = flip Map.lookup catalog

{- |


>>> take 7 $ availableOrgans
[Nothing,Just Heart,Nothing,Nothing,Nothing,Nothing,Just Heart]
-}
availableOrgans :: [Maybe Organ]
availableOrgans = getDrawerContents possibleDrawers organCatalog

{- |

>>> countOrgan Brain availableOrgans
1
>>> countOrgan Spleen availableOrgans
2
-}
countOrgan :: Organ -> [Maybe Organ] -> Int
countOrgan organ = length . filter isJust . filter (== Just organ)

data Container = Vat Organ | Cooler Organ | Bag Organ

instance Show Container where
    show :: Container -> String
    show (Vat organ) = show organ ++ " in a vat"
    show (Cooler organ) = show organ ++ " in a cooler"
    show (Bag organ) = show organ ++ " in a bag"

data Location = Lab | Kitchen | Bathroom deriving (Show)

organToContainer :: Organ -> Container
organToContainer = \case
    Brain -> Vat Brain
    Heart -> Cooler Heart
    organ -> Bag organ

placeInLocation :: Container -> (Location, Container)
placeInLocation = \case
    Vat a -> (Lab, Vat a)
    Cooler a -> (Lab, Cooler a)
    Bag a -> (Kitchen, Bag a)

{- |

>>> process Brain
(Lab,Brain in a vat)

>>> process Heart
(Lab,Heart in a cooler)
-}
process :: Organ -> (Location, Container)
process organ = placeInLocation $ organToContainer organ

{- |

>>> report $ process Brain
"Brain in a vat in the Lab"
-}
report :: (Location, Container) -> String
report (location, container) = show container ++ " in the " ++ show location

processAndReport :: Maybe Organ -> String
processAndReport (Just organ) = report $ process organ
processAndReport Nothing = "error: id not found"

{- |

>>> processRequest 1 organCatalog
"error: id not found"

>>> processRequest 7 organCatalog
"Heart in a cooler in the Lab"
-}
processRequest :: Int -> Map Int Organ -> String
processRequest id' catalog =
    processAndReport $
        Map.lookup id' catalog

{- | Q19.1

>>> emptyDrawers [Nothing, Just Heart, Nothing]
2
-}
emptyDrawers :: [Maybe Organ] -> Int
emptyDrawers = length . filter isNothing

{- | Q19.2
>>> maybeMap (*2) [Just 1, Just 2, Nothing, Just 4]
[Just 2,Just 4,Nothing,Just 8]
-}
maybeMap :: (a -> b) -> [Maybe a] -> [Maybe b]
maybeMap _ [] = []
maybeMap f (mx : xs) = case mx of
    Nothing -> Nothing : maybeMap f xs
    Just x -> Just (f x) : maybeMap f xs
