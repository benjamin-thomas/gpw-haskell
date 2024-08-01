{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Lib (isPalindrome)

quote :: Text -> Text
quote str = "'" <> str <> "'"

main :: IO ()
main = do
    TIO.putStrLn "Enter a word:"
    word <- TIO.getLine
    let verdict =
            if isPalindrome word
                then
                    T.unwords ["The word", quote word, "is a palindrome!"]
                else
                    T.unwords ["The word", quote word, "is NOT a palindrome!"]
    TIO.putStrLn verdict