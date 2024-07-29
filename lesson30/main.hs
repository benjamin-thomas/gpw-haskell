{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <&>" #-}
import Control.Monad ((>=>))
import Data.Map (Map)
import Data.Map qualified as Map

{- Off track explorations, for recap

Put an element in a a list

>>> (:) <$> Just 1 <*> Just []
Just [1]

We can repeat this operation `(:) <$> ITEM_IN_CONTAINER <*>` ITEMS_IN_CONTAINER

>>> (:) <$> Just 2 <*> ((:) <$> Just 1 <*> Just [])
Just [2,1]

>>> (:) <$> Just 3 <*> ((:) <$> Just 2 <*> ((:) <$> Just 1 <*> Just []))
Just [3,2,1]

---

That's basically folding in a context!

>>> foldl (\acc x -> (:) <$> x <*> acc) (Just []) [Just 1, Just 2, Just 3]
Just [3,2,1]

>>> foldr (\x acc -> (:) <$> x <*> acc) (Just []) [Just 1, Just 2, Just 3]
Just [1,2,3]

AKA liftA2!

I found this definition for `liftA2`

> liftA2 is for conveniently lifting a regular function of two arguments into a
> function operating on two applicative values.

>>> import Control.Applicative (liftA2)
>>> foldr (liftA2 (:)) (Just []) [Just 1, Just 2, Just 3]
Just [1,2,3]

 -}

type UserName = String
type GamerId = Int
type PlayerCredits = Int

userNameDB :: Map GamerId UserName
userNameDB =
    Map.fromList
        [ (1, "Steve")
        , (2, "Alex")
        , (3, "Simon")
        , (4, "Lee")
        , (5, "Judy")
        , (6, "Dave")
        ]

creditsDB :: Map UserName PlayerCredits
creditsDB =
    Map.fromList
        [ ("Steve", 500)
        , ("Alex", 2000)
        , ("Simon", 3000)
        , ("Lee", 5000)
        , ("Judy", 7000)
        , ("David", 0)
        ]

lookupUserName :: GamerId -> Maybe UserName
lookupUserName = (`Map.lookup` userNameDB)

lookupPlayerCredits :: UserName -> Maybe PlayerCredits
lookupPlayerCredits = (`Map.lookup` creditsDB)

{-

To chain these functions (computation), we need a function that goes

- from `Maybe UserName -> (UserName -> Maybe PlayerCredits)` to
- to `Maybe PlayerCredits`
 -}

when :: Maybe UserName -> (UserName -> Maybe PlayerCredits) -> Maybe PlayerCredits
when Nothing _ = Nothing
when (Just userName) f = f userName

{-

>>> creditsFor 1 -- OK
Just 500

>>> creditsFor 6 -- no credit
Nothing

>>> creditsFor 99 -- no user
Nothing

 -}
creditsFor :: GamerId -> Maybe PlayerCredits
creditsFor gamerId =
    let userName :: Maybe UserName = lookupUserName gamerId
     in when userName lookupPlayerCredits

{-

Considering:

when :: Maybe UserName -> (UserName -> Maybe PlayerCredits) -> Maybe PlayerCredits

We should really think more abstractly:

when :: Monad m => m a -> (a -> m b) -> m b
 -}

echo :: IO ()
echo = getLine >>= putStrLn

type WillCoId = Int

willCoDB :: Map WillCoId GamerId
willCoDB =
    Map.fromList
        [ (1001, 1)
        , (1002, 2)
        , (1003, 3)
        , (1004, 99)
        , (1006, 6)
        ]

lookupGamerId :: WillCoId -> Maybe GamerId
lookupGamerId = (`Map.lookup` willCoDB)

{-

>>> creditsFor2 1001
Just 500

>>> creditsFor2 1004 -- no gamerId
Nothing

>>> creditsFor2 1006 -- no user
Nothing

>>> creditsFor2 123 -- no willCoId
Nothing
 -}
creditsFor2 :: WillCoId -> Maybe PlayerCredits
creditsFor2 willCoId =
    lookupGamerId willCoId
        >>= lookupUserName
        >>= lookupPlayerCredits

{-
We can make it point free with Kleisli arrows
>>> creditsFor3 1001
Just 500

>>> creditsFor3 1004 -- no gamerId
Nothing

>>> creditsFor3 1006 -- no user
Nothing

>>> creditsFor3 123 -- no willCoId
Nothing
 -}
creditsFor3 :: WillCoId -> Maybe PlayerCredits
creditsFor3 =
    lookupGamerId
        >=> lookupUserName
        >=> lookupPlayerCredits

-- Quick check 30.3 Combine readInt and printDouble (defined next) into a single IO action:
readInt :: IO Int
readInt = read <$> getLine

printDbl :: Int -> IO ()
printDbl n = print (n * 2)

readAndPrint :: IO ()
readAndPrint = readInt >>= printDbl

echoVerbose :: IO ()
echoVerbose =
    putStrLn "Enter something to echo"
        >> getLine
        >>= putStrLn

-- Tie it all together
askForName :: IO ()
askForName = putStrLn "What is your name?"

greet :: String -> String
greet name = "Hello, " <> name <> "!"

helloName :: IO ()
helloName =
    askForName
        >> getLine
        >>= (return . greet)
        >>= putStrLn

{-
Q30.1

To prove that Monad is strictly more powerful than Functor, write a  universal
version of <$>, as in the preceding lesson’s exercise, called allFmapM, that
defines <$> for all members of the Monad type class

 -}

allFmapM :: (Monad m) => (a -> b) -> m a -> m b
allFmapM f ma = ma >>= return . f

{-

Q30.2 To prove that Monad is strictly more powerful than Applicative, write a universal
version of <*>, called allApp, that defines <*> for all members of the Monad type class.

 -}

allApp :: (Monad m) => m (a -> b) -> m a -> m b
allApp mf ma =
    mf >>= \f ->
        ma >>= return . f

{-

Q30.3 Implement a bind function which is the same as (>>=) for Maybe:

 -}

bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing _ = Nothing
bind (Just x) f = f x