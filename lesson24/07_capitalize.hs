{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Char qualified as C
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Environment (getArgs)

{-
Q24.2 Write a program the takes reads a file, then rewrites its content, capitalized.

 -}

params :: [String] -> Maybe String
params =
    \case
        [filepath] -> Just filepath
        _ -> Nothing

{- |

>>> capitalizeWord "hello world"
"Hello world"
-}
capitalizeWord :: Text -> Text
capitalizeWord x =
    case T.uncons x of
        Nothing -> x
        Just (c, xs) ->
            T.cons (C.toUpper c) xs

{- |

>>> capitalize "hello world once more"
"Hello World Once More"
-}
capitalize :: Text -> Text
capitalize txt = T.unwords $ map capitalizeWord (T.words txt)

main :: IO ()
main = do
    args <- getArgs
    case params args of
        Nothing -> putStrLn "Usage: capitalize filepath"
        Just filepath -> do
            content <- TIO.readFile filepath
            let newContent = capitalize content
            TIO.writeFile filepath newContent
    putStrLn "OK"
