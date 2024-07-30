{-# LANGUAGE LambdaCase #-}

import Data.List (intercalate)
import Text.Printf (printf)

{-

q32.1 Use a list comprehension that generates a list of correct calendar dates

 -}

data Month
    = Jan
    | Feb
    | Mar
    | Apr
    | May
    | Jun
    | Jul
    | Aug
    | Sep
    | Oct
    | Nov
    | Dec
    deriving (Show, Enum)

newtype Year = Year Int deriving (Enum)
instance Show Year where
    show :: Year -> String
    show (Year year) = printf "%04d" year

newtype Day = Day Int deriving (Enum)
instance Show Day where
    show :: Day -> String
    show (Day day) = printf "%02d" day

data Date = Date
    { year :: Year
    , month :: Month
    , day :: Day
    }

instance Show Date where
    show :: Date -> String
    show date =
        intercalate
            "-"
            [ show (year date)
            , show (month date)
            , show (day date)
            ]

{-

https://scienceworld.wolfram.com/astronomy/LeapYear.html#:~:text=The%20complete%20list%20of%20leap,2040%2C%202044%2C%20and%202048.

ghci> takeWhile (\n -> n < 2050) $ filter isLeapYear [2000 ..]
[2000,2004,2008,2012,2016,2020,2024,2028,2032,2036,2040,2044,2048]

 -}
isLeapYear :: Int -> Bool
isLeapYear year =
    (divBy 4 && not (divBy 100)) || divBy 400
  where
    divBy n = year `mod` n == 0

lastDay :: Year -> Month -> Int
lastDay (Year year) = \case
    Jan -> 31
    Feb -> if isLeapYear year then 29 else 28
    Mar -> 31
    Apr -> 30
    May -> 31
    Jun -> 30
    Jul -> 31
    Aug -> 31
    Sep -> 30
    Oct -> 31
    Nov -> 30
    Dec -> 31

dates :: [Date]
dates =
    [ Date{year = year, month = month, day = Day day}
    | year <- [Year 0 ..]
    , month <- [Jan ..]
    , day <- [1 .. lastDay year month]
    ]

{-

+ leap years
ghci> mapM_ print $ take 62 $ drop (365*2024+491) dates
2024-Jan-01
2024-Jan-02
2024-Jan-03
2024-Jan-04
2024-Jan-05
2024-Jan-06
2024-Jan-07
2024-Jan-08
...
2024-Jan-27
2024-Jan-28
2024-Jan-29
2024-Jan-30
2024-Jan-31
2024-Feb-01
2024-Feb-02
2024-Feb-03
...
2024-Feb-25
2024-Feb-26
2024-Feb-27
2024-Feb-28
2024-Feb-29
2024-Mar-01
2024-Mar-02
 -}

{-

q32.2 Now use do notation, and then use operators + lambdas

dates :: [Date]
dates =
    [ Date{year = year, month = month, day = Day day}
    | year <- [Year 0 ..]
    , month <- [Jan ..]
    , day <- [1 .. lastDay year month]
    ]

 -}

dates2 :: [Date]
dates2 = do
    year <- [Year 0 ..]
    month <- [Jan ..]
    day <- [1 .. lastDay year month]
    return $
        Date
            { year = year
            , month = month
            , day = Day day
            }

{- FOURMOLU_DISABLE -}
dates3 :: [Date]
dates3 =
    [Year 0 ..]               >>= \year ->
    [Jan ..]                  >>= \month ->
    [1 .. lastDay year month] >>= \day ->
    return $
        Date
            { year = year
            , month = month
            , day = Day day
            }
{- FOURMOLU_ENABLE -}