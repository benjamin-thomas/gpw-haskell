module Main (main) where

import qualified AesonBasics
import qualified NOAA

main :: IO ()
main = do
    putStrLn "Hello, Haskell!✓"
    AesonBasics.main
    NOAA.main
