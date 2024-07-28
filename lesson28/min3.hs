minOf3 :: (Ord a) => a -> a -> a -> a
minOf3 a b c = min a $ min b c

_minOf3 :: (Ord a) => a -> a -> a -> a
_minOf3 a b c = minimum [a, b, c]

readInt :: IO Int
readInt = read <$> getLine

minOfInts :: IO Int
minOfInts = minOf3 <$> readInt <*> readInt <*> readInt

main :: IO ()
main = do
    putStrLn "Enter three numbers"
    minInt <- minOfInts
    putStrLn (show minInt <> " is the smallest")
    putStrLn "New enter a user (str, int, int)"
    user <- ioUser
    print user

-- Quick check 28.4
-- >>> _a28_4
-- Just 3
_a28_4 :: Maybe Integer
_a28_4 = minOf3 <$> Just 10 <*> Just 3 <*> Just 6

data User = User
    { usrName :: String
    , usrGameId :: Int
    , usrScore :: Int
    }
    deriving (Show)

mbUser :: Maybe User
mbUser = User <$> Just "Sue" <*> Just 1337 <*> Just 9001

ioUser :: IO User
ioUser = User <$> getLine <*> readInt <*> readInt
