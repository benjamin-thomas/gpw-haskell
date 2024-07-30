-- Q31.1
-- Rewrite mainDo with operators
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Data.Map (Map)
import Data.Map qualified as Map

{-# HLINT ignore "Avoid lambda" #-}
mainDo :: IO ()
mainDo = do
    putStrLn "What is the size of pizza 1"
    size1 <- getLine
    putStrLn "What is the cost of pizza 1"
    cost1 <- getLine
    putStrLn "What is the size of pizza 2"
    size2 <- getLine
    putStrLn "What is the cost of pizza 2"
    cost2 <- getLine
    let pizza1 = (read size1, read cost1)
    let pizza2 = (read size2, read cost2)
    let betterPizza = comparePizzas pizza1 pizza2
    putStrLn (describePizza betterPizza)

{- FOURMOLU_DISABLE -}
main :: IO ()
main =
    putStrLn "What is the size of pizza 1" >>
    getLine >>= \size1 ->
    putStrLn "What is the cost of pizza 1" >>
    getLine >>= \cost1 ->
    putStrLn "What is the size of pizza 2" >>
    getLine >>= \size2 ->
    putStrLn "What is the cost of pizza 2" >>
    getLine >>= \cost2 ->
    let pizza1 = (read size1, read cost1)
        pizza2 = (read size2, read cost2)
        betterPizza = comparePizzas pizza1 pizza2
     in
    putStrLn (describePizza betterPizza)
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
main2 :: IO ()
main2 =
    putStrLn "What is the size of pizza 1" >>
    getLine >>= \size1 ->
    putStrLn "What is the cost of pizza 1" >>
    getLine >>= \cost1 ->
    putStrLn "What is the size of pizza 2" >>
    getLine >>= \size2 ->
    putStrLn "What is the cost of pizza 2" >>
    getLine >>= \cost2 ->
    (\pizza1 ->
        (\pizza2 ->
            (\betterPizza ->
                putStrLn (describePizza betterPizza)
            )
            (comparePizzas pizza1 pizza2)
        )
        (read size2, read cost2)
    )
    (read size1, read cost1)
{- FOURMOLU_ENABLE -}

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

type Pizza = (Double, Double) -- (diameter, cost)

costPerSqInch :: Pizza -> Double
costPerSqInch (diameter, cost) = cost / circleArea diameter

circleArea :: Double -> Double
circleArea diameter = pi * (diameter / 2) ^ (2 :: Integer)

-------- Q31.2
-- Rewrite this function so it works with the List type.
-- Don't worry if the returned data is "strange"

costData :: Map Int Double
costData = Map.fromList [(1, 18.0), (2, 16.0)]

sizeData :: Map Int Double
sizeData = Map.fromList [(1, 20.0), (2, 15.0)]

{-

>>> maybeMain
Just "The 20.0 inch pizza is cheaper at cost 5.729577951308232e-2 per square inch"

 -}
maybeMain :: Maybe String
maybeMain = do
    size1 <- Map.lookup 1 sizeData
    cost1 <- Map.lookup 1 costData
    size2 <- Map.lookup 2 sizeData
    cost2 <- Map.lookup 2 costData
    let pizza1 = (size1, cost1)
    let pizza2 = (size2, cost2)
    let betterPizza = comparePizzas pizza1 pizza2
    return (describePizza betterPizza)

listMain :: [String]
listMain = do
    (_, size1) <- Map.toList sizeData
    (_, cost1) <- Map.toList costData
    (_, size2) <- Map.toList sizeData
    (_, cost2) <- Map.toList costData
    let pizza1 = (size1, cost1)
    let pizza2 = (size2, cost2)
    let betterPizza = comparePizzas pizza1 pizza2
    return (describePizza betterPizza)

{-

Q31.3 Refactor the maybeMain function from the preceding exercise so that it works with
any Monad. You’ll need to change the type signature as well as remove the type-specific
parts from the body of the function.
 -}

monadMain ::
    (Monad m) =>
    (m Double, m Double) ->
    (m Double, m Double) ->
    m String
monadMain (getSize1, getCost1) (getSize2, getCost2) = do
    size1 <- getSize1
    cost1 <- getCost1
    size2 <- getSize2
    cost2 <- getCost2
    let pizza1 = (size1, cost1)
    let pizza2 = (size2, cost2)
    let betterPizza = comparePizzas pizza1 pizza2
    return (describePizza betterPizza)

{-

>>> maybeMain == maybeMain2
True

 -}
maybeMain2 :: Maybe String
maybeMain2 =
    monadMain
        (Map.lookup 1 sizeData, Map.lookup 1 costData)
        (Map.lookup 2 sizeData, Map.lookup 2 costData)

{-

>>> listMain == listMain2
True

 -}
listMain2 :: [String]
listMain2 =
    monadMain
        ( map snd $ Map.toList sizeData
        , map snd $ Map.toList costData
        )
        ( map snd $ Map.toList sizeData
        , map snd $ Map.toList costData
        )
