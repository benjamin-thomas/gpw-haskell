{-
Quick check 29.1

>>> (++) <$> Just "Inter" <*> Just "galactic"
Just "Intergalactic"

Or use the most abstract code
>>> (<>) <$> pure "Inter" <*> pure "galactic"
"Intergalactic"
 -}

{-
Pure

>>> (6+) <$> Just 4
Just 10

Same as:
>>> Just (6+) <*> Just 4
Just 10

Same as:
>>> pure (6+) <*> pure 4
10

Consider this:

ghci> :t pure (6+)
pure (6+) :: (Applicative f, Num a) => f (a -> a)

ghci> :t Just (6+)
Just (6+) :: Num a => Maybe (a -> a)
 -}

-- Quick check 29.2
helloWorld :: IO String
helloWorld = pure "Hello World"

{-
(,) is an instance of Functor (and NOT Applicative)

>>> (+1) <$> (0, 0)
(0,1)

>>> (+1) <$> (+1) <$> (0, 0)
(0,2)

Map is also an instance of Functor (and NOT Applicative)
>>> (+1) <$> Data.Map.fromList [('A', 1), ('B', 2)]
fromList [('A',2),('B',3)]

>>> (\_ -> 99) <$> Data.Map.fromList [('A', 1), ('B', 2)]
fromList [('A',99),('B',99)]

(,,) is also an instance of Functor (and NOT Applicative)
>>> (,,) 1 2 3
(1,2,3)

>>> (+1) <$> (0,0,0)
(0,0,1)

>>> (+1) <$> (+1) <$> (0,0,0)
(0,0,2)
 -}

{-
Quick check 29.3

So list can be a container (think Functor)

>>> (+1) <$> [1,2,3]
[2,3,4]

But list can also be a context (think Applicative)

(not I can't do this: pure (+1) <*> (0, 0))

>>> pure (+1) <*> [1,2,3]
[2,3,4]

But, really, applicative for lists means we apply a function to all possible
combinations of values (AKA a Cartesian product)

>>> pure (+) <*> [1,2] <*> [3,4]
[4,5,5,6]

Same as:
>>> (+) <$> [1,2] <*> [3,4]
[4,5,5,6]

In summary:

- A list as a CONTAINER is a sequence of values that can hold any one type
- A list as a CONTEXT represents a set of possibilities.

 -}

{-
Not very efficient implementation

>>> primesToN 100
[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]

 -}
primesToN :: Integer -> [Integer]
primesToN n = filter isNotComposite twoThroughN
  where
    twoThroughN = [2 .. n]
    composite = (*) <$> twoThroughN <*> twoThroughN
    isNotComposite = not . (`elem` composite)

-- Generating large amount of test data
data User = User
    { usrName :: String
    , usrGamerId :: Int
    , usrScore :: Int
    }
    deriving (Show)

-- Let's say we want to generate combinations of potentially problematic values

testNames :: [String]
testNames =
    [ "John Smith"
    , "Bobby '); DROP TABLE users; --"
    , "Christina NULL"
    , "John ' OR '1' = '1"
    ]

testIds :: [Int]
testIds =
    [ 1337
    , 0123
    , 999_999
    ]

testScores :: [Int]
testScores =
    [ 0
    , 100_000
    , -99_999
    ]

{-

This create 36 test users (4 names * 3 ids * 3 scores)

ghci> mapM_ print $ take 3 testData
User {usrName = "John Smith", usrGamerId = 1337, usrScore = 0}
User {usrName = "John Smith", usrGamerId = 1337, usrScore = 100000}
User {usrName = "John Smith", usrGamerId = 1337, usrScore = -99999}

 -}
testData :: [User]
testData =
    User
        <$> testNames
        <*> testIds
        <*> testScores
