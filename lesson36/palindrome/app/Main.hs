{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import MyLib (isPalindrome)

quote :: Text -> Text
quote str = "'" <> str <> "'"

main :: IO ()
main = do
  TIO.putStrLn "Enter word"
  word <- TIO.getLine
  let result =
        if isPalindrome word
          then T.unwords ["The word", quote word, "IS a palindrome!"]
          else T.unwords ["The word", quote word, "is NOT a palindrome!"]
  TIO.putStrLn result
