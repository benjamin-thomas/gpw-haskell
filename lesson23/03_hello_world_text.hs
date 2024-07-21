{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import Data.Text.IO qualified as TIO

{-
Q23.1

Rewrite the previous hello world exercise with the `Text` type, rather than `String`.
 -}

greetings :: Text -> Text
greetings name = "Hello " <> " " <> name <> "!"

main :: IO ()
main = do
    TIO.putStrLn "Hello, what's your name?"
    name <- TIO.getLine
    TIO.putStrLn $ greetings name
