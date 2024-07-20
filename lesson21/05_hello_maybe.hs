-- Q21.1
-- ghci ./lesson21/05_hello_maybe.hs

-- cabal install --lib random
-- cabal install --lib containers
import Data.Map (Map)
import Data.Map qualified as Map

greetings :: String -> String
greetings name = "Hello, " ++ name ++ "!"

names :: Map Int String
names = Map.fromList [(1, "Benjamin")]

maybeMain :: Maybe String
maybeMain = do
    name <- Map.lookup 1 names
    let greet = greetings name
    return greet
