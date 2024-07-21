{-# LANGUAGE LambdaCase #-}

import Data.Text.IO qualified as TIO
import System.Environment (getArgs)

{-
Q24.1 Write a simple version of `cp`

 -}

params :: [String] -> Maybe (String, String)
params =
    \case
        [from, to] -> Just (from, to)
        _ -> Nothing

main :: IO ()
main = do
    args <- getArgs
    case params args of
        Nothing -> putStrLn "Usage: cp from to"
        Just (from, to) ->
            TIO.writeFile to =<< TIO.readFile from

    putStrLn "OK"