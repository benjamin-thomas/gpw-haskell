module Lesson04 where

import Data.Function (on)
import Data.List (sort, sortBy)
import Data.Tuple (swap)

names :: [(String, String)]
names =
    [ ("John", "Doe")
    , ("Jane", "Doe")
    , ("Alice", "Doe")
    , ("Bob", "Doe")
    , ("Will", "Kurt")
    , ("Graham", "Hutton")
    , ("Alice", "Adams")
    ]

{- |
>>> sorted
[("Alice","Adams"),("Alice","Doe"),("Bob","Doe"),("Graham","Hutton"),("Jane","Doe"),("John","Doe"),("Will","Kurt")]
-}
sorted = sort names

{- |
>>> compareLastNames ("Alice","Adams") ("Bob","Doe")
LT

>>> compareLastNames ("Bob","Doe") ("Alice","Adams")
GT

>>> compareLastNames ("John","Doe") ("Jane","Doe")
GT

>>> compareLastNames ("Jane","Doe") ("John","Doe")
LT

>>> compareLastNames ("Alice","Doe") ("Alice","Doe")
EQ
-}

-- compareLastNames :: (String, String) -> (String, String) -> Ordering
-- compareLastNames (fa, la) (fb, lb) =
--     case byLastName of
--         EQ -> compare fa fb
--         _ -> byLastName
--   where
--     byLastName = compare la lb
--
-- compareLastNames :: (String, String) -> (String, String) -> Ordering
-- compareLastNames (fa, la) (fb, lb) = compare (la, fa) (lb, fb)
--

compareLastNames :: (String, String) -> (String, String) -> Ordering
-- compareLastNames a b = compare (swap a) (swap b)
compareLastNames = on compare swap

{- |
>>> sortedByLastName names
[("Alice","Adams"),("Alice","Doe"),("Bob","Doe"),("Jane","Doe"),("John","Doe"),("Graham","Hutton"),("Will","Kurt")]
-}
sortedByLastName :: [(String, String)] -> [(String, String)]
sortedByLastName = sortBy compareLastNames
