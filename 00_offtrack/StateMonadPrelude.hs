{-# OPTIONS_GHC -Wall #-}

module StateMonadPrelude (main) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.State (StateT, execStateT, get, put)

{-

ghcid ./StateMonadPrelude.hs --test=:main

 -}

data AppState = AppState
    { foo :: Int
    , bar :: Int
    }
    deriving (Show)

run :: StateT AppState IO ()
run = do
    liftIO $ putStrLn "Entering app..."
    put $ AppState{foo = 1, bar = 2}
    st <- get
    if foo st == 1
        then
            liftIO $ putStrLn "Foo is 1"
        else
            liftIO $ putStrLn "Foo is not 1"
    liftIO $ putStrLn "App state changed"

main :: IO ()
main = do
    putStrLn "Booting up..."
    finalState <-
        execStateT run (AppState{foo = 0, bar = 0})
    putStrLn "Shutting down..."
    print finalState
