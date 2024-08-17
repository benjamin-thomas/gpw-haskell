{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

module StateMonadRelude (main) where

import Relude

{-

ghcid ./StateMonadRelude.hs --test=:main

 -}

data AppState = AppState
    { foo :: Int
    , bar :: Int
    , msg :: String
    }
    deriving (Show)

run :: StateT AppState IO ()
run = do
    putStrLn "Entering app..."
    (\st -> putStrLn $ "1) App state is: " <> show st) =<< get
    put $ AppState{foo = 1, bar = 2, msg = "Goodbye!"}
    st1 <- get -- I put `st1` into scope for demonstration purposes, although not really necessary
    putStrLn $ "2) App state is: " <> show st1
    st2 <- get
    if foo st2 == 1
        then
            putStrLn "Foo is 1"
        else
            putStrLn "Foo is not 1"
    putStrLn "App state changed"

main :: IO ()
main = do
    putStrLn "Booting up..."
    finalState <-
        execStateT run (AppState{foo = 0, bar = 0, msg = "Hello!"})
    putStrLn "Shutting down..."
    print finalState
