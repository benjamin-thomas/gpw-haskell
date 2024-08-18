module Main where

import System.Environment.Blank (getEnv)

main :: IO ()
main = do
    mToken <- getEnv "TOKEN"
    case mToken of
        Nothing -> putStrLn "Token is missing, check the README"
        Just token -> putStrLn $ "Your token is: " <> token
