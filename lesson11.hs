module Lesson11 where

import Text.Read (readMaybe)

{- | Conversions

>>> intToString 123
"123"

>>> stringToInt "123"
Just 123

>>> stringToInt "abc"
Nothing

>>> read "234" :: Integer
234
-}
intToString :: Integer -> String
intToString = show

stringToInt :: String -> Maybe Integer
stringToInt = readMaybe
