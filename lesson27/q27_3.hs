import Data.Map (Map)
import Data.Map qualified as Map
import System.Environment (getArgs)

partsDB :: Map Int RobotPart
partsDB =
    Map.fromList
        [ (1, leftArm)
        , (2, rightArm)
        , (3, robotHead)
        ]

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

-- main :: IO ()
-- main = do
--     args <- getArgs
--     let idx = (read $ head args) :: Int
--     case Map.lookup idx partsDB of
--         Nothing -> putStrLn "Not found"
--         Just robot -> print robot

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

main :: IO ()
main = do
    mStr <- safeHead <$> getArgs
    let mInt :: Maybe Int = read <$> mStr
    case mInt of
        Nothing ->
            putStrLn "Must provide an index!"
        Just idx -> do
            let mPart = Map.lookup idx partsDB
            case mPart of
                Nothing ->
                    putStrLn $
                        "No robot found with that key"
                            <> " "
                            <> "("
                            <> show idx
                            <> ")"
                Just robot -> print robot