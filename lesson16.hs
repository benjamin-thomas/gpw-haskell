{-# LANGUAGE LambdaCase #-}

module Lesson16 where

-- Product types
data Author = AuthorName String String
data Book = Author String String Int

data FullBook = FullBook
    { author :: Author
    , isbn :: String
    , title :: String
    , year :: Int
    , bookPrice :: Double
    }

data VinylRecord = VinylRecord
    { artist :: Author
    , recordTitle :: String
    , recordYear :: String
    , recordPrice :: Double
    }

data Toy = Toy
    { name :: String
    , descr :: String
    , toyPrice :: Double
    }

data StoreItem
    = BookItem FullBook
    | RecordItem VinylRecord
    | ToyItem Toy

{- | To prevent names clashing, we must prepend each attribute with a unique identifier.
So price becomes bookPrice, and so on...
-}
price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy

-- | Q16.1 not interesting

-- | Q16.2 implement Shape etc.
data Shape
    = Circle {radius :: Float}
    | Square {side :: Float}
    | Rectangle {width :: Float, height :: Float}

{- |

>>> area $ Circle 4
50.265484

>>> area $ Square 5
25.0

>>> area $ Rectangle 3 4
12.0
-}
area :: Shape -> Float
area = \case
    Circle{radius} -> pi * radius * radius
    Square{side} -> side * side
    Rectangle{width, height} -> width * height
