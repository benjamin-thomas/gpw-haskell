{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

{-
Q23.2

Rewrite the previous summing exercise with the `Text` type, rather than `String`.
 -}

toInts :: Text -> [Int]
toInts = map (read . T.unpack) . T.lines

main :: IO ()
main = do
    userInput <- TIO.getContents
    let total = sum $ toInts userInput
    print $ T.pack (show total)
