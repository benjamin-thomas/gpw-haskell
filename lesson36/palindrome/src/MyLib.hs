module MyLib (isPalindrome, clean) where

import Data.Char (isPunctuation)
import Data.Text (Text)
import Data.Text qualified as T

clean :: Text -> Text
clean = T.filter (not . isPunctuation)

isPalindrome :: Text -> Bool
isPalindrome str = cleaned == T.reverse cleaned
  where
    cleaned = clean str
