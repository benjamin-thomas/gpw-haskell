newtype Book
    = Book {bookId :: Int}
    deriving (Show)

newtype Game
    = Game {gameDifficulty :: Float}
    deriving (Show)

newtype Box a
    = Box a
    deriving (Show)

instance Functor Box where
    fmap :: (a -> b) -> Box a -> Box b
    fmap f (Box v) = Box (f v)

present :: Box Book
present = Box $ Book{bookId = 1}

game :: Box Game
game = Box $ Game{gameDifficulty = 0.8}

moreOfIt :: Box a -> Box [a]
-- morePresents x = replicate 10 <$> x
moreOfIt = fmap $ replicate 3

presents :: Box [Book]
presents = moreOfIt present

games :: Box [Game]
games = moreOfIt game

{-

>>> present
Box (Book {bookId = 1})

>>> presents
Box [Book {bookId = 1},Book {bookId = 1},Book {bookId = 1}]

>>> games
Box [Game {gameDifficulty = 0.8},Game {gameDifficulty = 0.8},Game {gameDifficulty = 0.8}]

 -}
