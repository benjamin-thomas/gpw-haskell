{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

module StateMonadProtolude (main) where

import Protolude

{-

ghcid ./StateMonadProtolude.hs --test=:main

 -}

data AppState = AppState
    { foo :: Int
    , bar :: Int
    }
    deriving (Show)

run :: StateT AppState IO ()
run = do
    putStrLn "Entering app..."
    put $ AppState{foo = 1, bar = 2}
    st <- get
    if foo st == 1
        then
            putStrLn "Foo is 1"
        else
            putStrLn "Foo is not 1"
    putStrLn "App state changed"

main :: IO ()
main = do
    putStrLn "Booting up..."
    finalState <-
        execStateT run (AppState{foo = 0, bar = 0})
    putStrLn "Shutting down..."
    print finalState
