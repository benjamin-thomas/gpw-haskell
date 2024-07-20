{-

Q22.1 Add or multiply 2 numbers

doctest ./lesson22/08_simple_calc.hs
runghc ./lesson22/08_simple_calc.hs

 -}
{-# LANGUAGE LambdaCase #-}

data Operation n
    = (Num n) => Add [n]
    | (Num n) => Mul [n]

instance (Show n) => Show (Operation n) where
    show :: (Show n) => Operation n -> String
    show = \case
        Add xs -> "Add " ++ show xs
        Mul xs -> "Mul " ++ show xs

solve :: Operation n -> n
solve = \case
    Add xs -> sum xs
    Mul xs -> product xs

{- |
>>> solve (Add [2,3,4])
9

>>> solve (Mul [3,4])
12
-}

{- |

>>> toOperation "+ 2 3 4"
Add [2,3,4]

>>> toOperation "* 3 4"
Mul [3,4]

>>> solve $ toOperation "* 3 4"
12
-}
toOperation :: String -> Operation Int
toOperation s =
    case words s of
        "+" : lst -> Add (map read lst)
        "*" : lst -> Mul (map read lst)
        [] -> error "toOperation: empty list"
        _ -> error $ "toOperation: bad input => " ++ show s

toOperations :: String -> [Operation Int]
toOperations =
    map toOperation . lines

main :: IO ()
main = do
    userInput <- getContents
    let operations = toOperations userInput
    mapM_ (print . solve) operations
