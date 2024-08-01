{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-

Unless we activate these pragmas, we would define these types
in different modules, to avoid naming conflicts.

 -}

data Book
    = Book
    { title :: String
    , price :: Double
    }

book1 :: Book
book1 =
    Book
        { title = "Book 1"
        , price = 10
        }

-- >>> book1Title
-- "Book 1"
book1Title :: String
book1Title = book1.title

data Magazine
    = Magazine
    { title :: String
    , price :: Int
    }
