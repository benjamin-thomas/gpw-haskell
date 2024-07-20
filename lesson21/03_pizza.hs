{-

Run the program with:

    (FILE=./lesson21/03_pizza.hs; echo $FILE | entr -c bash -c "doctest $FILE && runghc -Wall $FILE")

This kind of workflow should push me to test pure functions (simply), and delegating the rest to IO.

> The more code you write that isn't in the `IO` context, the more code you know will never
> be vulnerable to I/O errors.
 -}

{- |

>>> circleArea 18
254.46900494077323
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

main :: IO ()
main = do
    putStrLn "What is the size of pizza 1?"
    size1 <- getLine
    putStrLn "What is the cost of pizza 1?"
    cost1 <- getLine
    putStrLn "What is the size of pizza 2?"
    size2 <- readLn
    putStrLn "What is the cost of pizza 2?"
    cost2 <- readLn
    let pizza1 = (read size1, read cost1)
    let pizza2 = (size2, cost2)
    let cheapest = comparePizzas pizza1 pizza2
    putStrLn $ describePizza cheapest