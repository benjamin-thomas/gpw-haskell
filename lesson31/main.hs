{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}

-- In the previous lesson, we saw:
askForName :: IO ()
askForName = putStrLn "What is your name?"

greet :: String -> String
greet name = "Hello, " ++ name ++ "!"

helloName :: IO ()
helloName =
    askForName
        >> getLine
        >>= ( \name ->
                return (greet name)
            )
        >>= putStrLn

-- Consider this...
{-

ghci> maxPairM $ (,) <$> readInt <*> readInt
5
8
8

ghci> maxPairM $ Just (5, 8)
Just 8

ghci> maxPairM $ [(5, 8), (3, 2)]
[8,3]

 -}
maxPairM :: (Monad m, Ord a) => m (a, a) -> m a
maxPairM ma = ma >>= (\(a, b) -> return (max a b))

readInt :: IO Int
readInt = read <$> getLine

helloNameDo :: IO ()
helloNameDo = do
    askForName
    name <- getLine
    putStrLn (greet name)

-- Quick check 31.1 Rewrite echo by using do-notation
echo :: IO ()
echo = do
    line <- getLine
    putStrLn line