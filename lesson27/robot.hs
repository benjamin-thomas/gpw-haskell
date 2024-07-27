import Control.DeepSeq (deepseq)
import Data.Map (Map)
import Data.Map qualified as Map
import Debug.Trace (trace)

data RobotPart = RobotPart
    { rbtName :: String
    , rbtDescr :: String
    , rbtCost :: Double
    , rbtCount :: Int
    }
    deriving (Show)

leftArm :: RobotPart
leftArm =
    RobotPart
        { rbtName = "left arm"
        , rbtDescr = "left arm for face punching!"
        , rbtCost = 1000.0
        , rbtCount = 3
        }

rightArm :: RobotPart
rightArm =
    RobotPart
        { rbtName = "right arm"
        , rbtDescr = "right arm for kind hand gestures"
        , rbtCost = 1025.0
        , rbtCount = 5
        }

robotHead :: RobotPart
robotHead =
    RobotPart
        { rbtName = "robot head"
        , rbtDescr = "this head looks mad"
        , rbtCost = 5092.25
        , rbtCount = 2
        }

type Html = String

renderHtml :: RobotPart -> Html
renderHtml part =
    trace ("Rendering HTML for: " ++ rbtName part) $
        mconcat
            [ mconcat
                [ "<h2>"
                , partName
                , "</h2>"
                ]
            , mconcat
                [ "<ul>"
                , mconcat ["<li>", partDesc, "</li>"]
                , mconcat ["<li>", partCost, "</li>"]
                , mconcat ["<li>", partCount, "</li>"]
                , "</ul>"
                ]
            ]
  where
    partName = rbtName part
    partDesc = rbtDescr part
    partCost = show (rbtCost part)
    partCount = show (rbtCount part)

partsDB :: Map Int RobotPart
partsDB =
    Map.fromList
        [ (1, leftArm)
        , (2, rightArm)
        , (3, robotHead)
        ]

partVal :: Maybe RobotPart
partVal = Map.lookup 1 partsDB

{-

Because `Maybe` is an instance of `Functor`, we can use `fmap`.

 -}
partHtml :: Maybe Html
partHtml = renderHtml <$> partVal

insertSnippet :: Maybe Html -> IO ()
insertSnippet _ =
    putStrLn "I could insert HTML in my HTTP response, or return an HTTP error..."

{- |

>>> allParts !! 0
RobotPart {rbtName = "left arm", rbtDescr = "left arm for face punching!", rbtCost = 1000.0, rbtCount = 3}

>>> allParts2 !! 0
RobotPart {rbtName = "left arm", rbtDescr = "left arm for face punching!", rbtCost = 1000.0, rbtCount = 3}
-}
allParts :: [RobotPart]
allParts =
    map snd (Map.toList partsDB)

-- Quick check 27.3
allParts2 :: [RobotPart]
allParts2 = snd <$> Map.toList partsDB

{-

Because `List` is also an instance of `Functor`, we can also use `fmap` (which
is equivalent to `map` in this case).

 -}
allPartsHtml :: [Html]
allPartsHtml = renderHtml <$> allParts

{-

To avoid continuously converting our robot to html, we can also easily convert
our whole DB in one go!

And since `Map` is an instance of `Functor`...

>>> Map.lookup 1 htmlPartsDB
Just "<h2>left arm</h2><ul><li>left arm for face punching!</li><li>1000.0</li><li>3</li></ul>"
 -}

htmlPartsDB :: Map Int Html
htmlPartsDB = renderHtml <$> partsDB

{-

Side note:

Because Haskell is lazy by default, that means our new Map is in fact just a
"description", the conversion happens "as-needed"! Wow!! 🤯️🤯️🤯️

ghci> length <$> Map.lookup 1 htmlPartsDB
Just Rendering HTML for: left arm
87
ghci> length <$> Map.lookup 1 htmlPartsDB
Just 87
ghci> length <$> Map.lookup 99 htmlPartsDB
Nothing
ghci> length <$> Map.lookup 2 htmlPartsDB
Just Rendering HTML for: right arm
93

---

To force all values to be evaluated as soon as possible, we can use `deepseq`

ghci> length <$> Map.lookup 1 htmlPartsDBStrict
Rendering HTML for: right arm
Rendering HTML for: left arm
Rendering HTML for: robot head
Just 87
ghci> length <$> Map.lookup 1 htmlPartsDBStrict
Just 87
ghci> length <$> Map.lookup 2 htmlPartsDBStrict
Just 93

 -}

htmlPartsDBStrict :: Map Int Html
htmlPartsDBStrict =
    x `deepseq` x
  where
    x = renderHtml <$> partsDB

leftArmIO :: IO RobotPart -- imagine it comes from a database
leftArmIO = return leftArm

{-

Because `IO` is an instance of `Functor`, again, we can use `fmap`!

 -}

htmlPartIO :: IO Html
htmlPartIO = renderHtml <$> leftArmIO

{-

In summary
==========

A Functor's <$> provides a common interface to apply any function (on the left)
to a value in a context (on the right).

 -}
