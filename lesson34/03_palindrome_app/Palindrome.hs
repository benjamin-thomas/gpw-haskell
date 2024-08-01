module Palindrome (isPalindrome) where

import Data.Text (Text)
import Data.Text qualified as T

import Data.Char (
  isPunctuation,
  isSpace,
  toLower,
 )

stripWhitespace :: Text -> Text
stripWhitespace = T.filter (not . isSpace)

stripPunctuation :: Text -> Text
stripPunctuation = T.filter (not . isPunctuation)

toLowerCase :: Text -> Text
toLowerCase = T.map toLower

clean :: Text -> Text
clean = stripWhitespace . stripPunctuation . toLowerCase

{- |

>>> :set -XOverloadedStrings

>>> isPalindrome "A man, a plan, a canal: Panama"
True

>>> isPalindrome "racecar"
True

>>> isPalindrome "hello"
False

>>> isPalindrome ""
True
-}
isPalindrome :: Text -> Bool
isPalindrome dirty =
  cleaned == T.reverse cleaned
 where
  cleaned = clean dirty
