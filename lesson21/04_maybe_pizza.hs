-- cabal install --lib containers
import Data.Map (Map)
import Data.Map qualified as Map

{-

Run the program with:

    (FILE=./lesson21/04_maybe_pizza.hs; echo $FILE | entr -c bash -c "doctest $FILE && runghc -Wall $FILE")

 -}

{- |

-- >>> circleArea 18
-- 254.46900494077323
--
-}
circleArea :: Double -> Double
circleArea diameter = pi * (diameter / 2) ^ (2 :: Integer)

type Pizza = (Double, Double) -- (diameter, cost)

{- |

>>> costPerSqInch (18, 20)
7.859503362562734e-2
-}
costPerSqInch :: Pizza -> Double
costPerSqInch (diameter, cost) = cost / circleArea diameter

{- |

>>> comparePizzas (18, 20) (18, 30)
(18.0,20.0)

>>> comparePizzas (18, 30) (18, 20)
(18.0,20.0)
-}
comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas a b =
    if costPerSqInch a < costPerSqInch b
        then a
        else b

describePizza :: Pizza -> String
describePizza (diameter, cost) =
    "The "
        ++ show diameter
        ++ " inch pizza is cheaper at cost "
        ++ show costPerSqInch'
        ++ " per square inch"
  where
    costPerSqInch' = costPerSqInch (diameter, cost)

costData :: Map Int Double
costData = Map.fromList [(1, 18.0), (2, 16.0)]

sizeData :: Map Int Double
sizeData = Map.fromList [(1, 20.0), (2, 15.0)]

maybeMain :: Maybe String
maybeMain = do
    size1 <- Map.lookup 1 sizeData
    cost1 <- Map.lookup 1 costData
    size2 <- Map.lookup 2 sizeData
    cost2 <- Map.lookup 2 costData
    let pizza1 = (size1, cost1)
    let pizza2 = (size2, cost2)
    let cheapest = comparePizzas pizza1 pizza2
    return $ describePizza cheapest

main :: IO ()
main = do
    print maybeMain
