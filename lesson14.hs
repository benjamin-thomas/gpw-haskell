{-# LANGUAGE LambdaCase #-}

module Lesson14 where

import Data.Function (on)
import Data.Tuple (swap)

data Die = One | Two | Three | Four | Five | Six

-- Manually implementing type classes
instance Show Die where
    show :: Die -> String
    show = \case
        One -> "I"
        Two -> "II"
        Three -> "III"
        Four -> "IV"
        Five -> "V"
        Six -> "VI"

instance Eq Die where
    One == One = True
    Two == Two = True
    Three == Three = True
    Four == Four = True
    Five == Five = True
    Six == Six = True
    _ == _ = False

instance Ord Die where
    compare Six Six = EQ
    compare _ Six = LT
    compare Six _ = GT
    compare Five Five = EQ
    compare _ Five = LT
    compare Five _ = GT
    compare Four Four = EQ
    compare _ Four = LT
    compare Four _ = GT
    compare Three Three = EQ
    compare _ Three = LT
    compare Three _ = GT
    compare Two Two = EQ
    compare _ Two = LT
    compare Two _ = GT
    compare One One = EQ

{- | The above implementation works because the first patterns have highest priority.

The last pattern is quite minimal, since already covered by the prior patterns.


>>> Data.List.sort [One, Three, Two, Five, Four, Six]
[I,II,III,IV,V,VI]
-}

-- When deriving `Ord`, the order of the type declaration is used to define the position.
data Asc
    = AAA
    | BBB
    deriving (Eq, Ord)

data Desc
    = DDD
    | CCC
    deriving (Eq, Ord)

{- |

>>> compare AAA BBB
LT

>>> compare CCC DDD
GT
-}

-- Implement `Enum`, which translates the type to and from Int
instance Enum Die where
    toEnum = \case
        1 -> One
        2 -> Two
        3 -> Three
        4 -> Four
        5 -> Five
        6 -> Six
        _ -> error "toEnum: bad value"

    fromEnum = \case
        One -> 1
        Two -> 2
        Three -> 3
        Four -> 4
        Five -> 5
        Six -> 6

{- |

>>> toEnum 6 :: Die
VI

>>> toEnum 7 :: Die
*** Exception: toEnum: bad value
...

>>> fromEnum One
1

>>> fromEnum Six
6

Now we can generate ranges!

>>> [One .. Six]
[I,II,III,IV,V,VI]

We get an error which we wouldn't have gotten if we used the deriving mechanisms
>>> [One ..]
[I,II,III,IV,V,VI,*** Exception: toEnum: bad value
...
-}
data Color
    = Red
    | Green
    | Blue
    deriving (Enum, Show)

{- |

>>> [Red .. ]
[Red,Green,Blue]

>>> (fromEnum Red, fromEnum Green, fromEnum Blue)
(0,1,2)

>>> (toEnum 0, toEnum 1, toEnum 2) :: (Color, Color, Color)
(Red,Green,Blue)

>>> toEnum 3 :: Color
*** Exception: toEnum{Color}: tag (3) is outside of enumeration's range (0,2)
...
-}

-- In lesson04, we defined a custom sortBy function (sortedByLastName = sortBy compareLastNames)
-- Now we'll do the same but customize `Ord` for our custom type.

newtype Name
    = Name (String, String)
    deriving (Eq, Show)

instance Ord Name where
    compare :: Name -> Name -> Ordering
    compare (Name a) (Name b) = compare (swap a) (swap b)

names :: [Name]
names =
    [ Name ("John", "Doe")
    , Name ("Jane", "Doe")
    , Name ("Alice", "Doe")
    , Name ("Bob", "Doe")
    , Name ("Will", "Kurt")
    , Name ("Graham", "Hutton")
    , Name ("Alice", "Adams")
    ]

{- |

>>> take 3 $ Data.List.sort names
[Name ("Alice","Adams"),Name ("Alice","Doe"),Name ("Bob","Doe")]

>>> (\(Name x) -> x) <$> Data.List.sort names
[("Alice","Adams"),("Alice","Doe"),("Bob","Doe"),("Jane","Doe"),("John","Doe"),("Graham","Hutton"),("Will","Kurt")]
-}

-- | Q14.1: implement `Eq` and `Ord` from a derived `Enum` instance
data Size
    = Small
    | Medium
    | Large
    | ExtraLarge
    deriving (Enum, Show)

instance Eq Size where
    (==) :: Size -> Size -> Bool
    -- a == b = fromEnum a == fromEnum b
    (==) = on (==) fromEnum

{- |

>>> Small == Small
True

>>> Small /= Large
True
-}

{- | Q14.2 declare FiveSidedDie and some related classes

See ./lesson14/q14.hs
-}
