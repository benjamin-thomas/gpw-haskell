{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module MyLib (
    addUser,
    doCheckin,
    checkout,
    getTools,
    getAvailableTools,
    getUnavailableTools,
    getUsers,
    updateUser,
) where

import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Data.Time (Day, UTCTime (utctDay), getCurrentTime)
import Database.SQLite.Simple (
    Connection,
    FromRow,
    Only (Only),
    Query,
    SQLData (SQLInteger, SQLText),
    close,
    execute,
    field,
    open,
    query,
    query_,
    toRow,
 )
import Database.SQLite.Simple.FromRow (FromRow (fromRow))
import Database.SQLite.Simple.ToRow (ToRow)
import GHC.Generics (Generic)
import Text.RawString.QQ (r)

withConn :: (Connection -> IO a) -> IO a
withConn f = do
    conn <- open "./tools.db"
    result <- f conn
    close conn
    return result

runQuery :: (ToRow q, FromRow r) => Query -> q -> IO [r]
runQuery q p = withConn $ \conn -> query conn q p

runQuery_ :: (FromRow r) => Query -> IO [r]
runQuery_ q = withConn $ \conn -> query_ conn q

execQuery :: (ToRow q) => Query -> q -> IO ()
execQuery q p = withConn $ \conn -> execute conn q p

data User = User
    { userId :: Int
    , userName :: String
    }

instance Show User where
    show user =
        unlines
            [ mconcat ["UserId : ", show $ userId user]
            , mconcat ["Name   : ", userName user]
            ]

instance FromRow User where
    fromRow =
        User
            <$> field
            <*> field

instance ToRow User where
    toRow user =
        [ SQLInteger $ fromIntegral $ userId user
        , SQLText $ T.pack $ userName user
        ]

addUser :: String -> IO ()
addUser userName' = do
    execQuery "INSERT INTO users (username) VALUES (?)" (Only userName')
    putStrLn "User added"

updateUser :: User -> IO ()
updateUser user = do
    execQuery "UPDATE users SET username = ? WHERE user_id = ?" (userName user, userId user)
    putStrLn "User updated!"

getUsers :: IO [User]
getUsers = runQuery_ "SELECT * FROM users"

{-

ghci> printUsers
UserId : 1
Name   : Benjamin
---
UserId : 2
Name   : john-doe
---
ghci> user = head <$> getUsers
ghci> updateUser =<< (\u -> u{userName="Ben"}) <$> user
User updated!
ghci> printUsers
UserId : 1
Name   : Ben
---
UserId : 2
Name   : john-doe
---
ghci> updateUser =<< (\u -> u{userId=99,userName="Nope"}) <$> user
User updated!
ghci> printUsers
UserId : 1
Name   : Ben
---
UserId : 2
Name   : john-doe
---
ghci> updateUser =<< (\u -> u{userId=2,userName="John"}) <$> user
User updated!
ghci> user
UserId : 1
Name   : Ben
---
ghci> printUsers
UserId : 1
Name   : Ben
---
UserId : 2
Name   : John
---
ghci>

 -}

data Tool
    = Tool
    { toolId :: Int
    , name :: String
    , descr :: String
    , lastReturned :: Maybe Day
    , timesBorrowed :: Int
    }
    deriving (Generic)

instance FromRow Tool
instance ToRow Tool

instance Show Tool where
    show tool =
        unlines
            [ mconcat ["ToolId         : ", show $ toolId tool]
            , mconcat ["Name           : ", name tool]
            , mconcat ["Description    : ", descr tool]
            , mconcat ["Last returned  : ", show $ lastReturned tool]
            , mconcat ["Times borrowed : ", show $ timesBorrowed tool]
            ]

getTools :: IO [Tool]
getTools = runQuery_ "SELECT * FROM tools"

getAvailableTools :: IO [Tool]
getAvailableTools =
    runQuery_
        [r|
            SELECT t.* FROM tools AS t
            LEFT JOIN checked_out AS c
            ON c.tool_id = t.tool_id
            WHERE c.tool_id IS NULL
        |]

getUnavailableTools :: IO [Tool]
getUnavailableTools =
    runQuery_
        [r|
            SELECT t.* FROM tools AS t
            LEFT JOIN checked_out AS c
            ON c.tool_id = t.tool_id
            WHERE c.tool_id IS NOT NULL
        |]

getToolById :: Int -> IO (Maybe Tool)
getToolById id' =
    listToMaybe
        <$> runQuery
            "SELECT * FROM tools WHERE tool_id = ?"
            (Only id')

newtype LastReturn = LastReturn (Maybe Day)
newtype TimesBorrowed = TimesBorrowed Int
newtype ToolId = ToolId Int

updateTool :: (LastReturn, TimesBorrowed, ToolId) -> IO ()
updateTool
    ( LastReturn lastReturned_
        , TimesBorrowed timesBorrowed_
        , ToolId toolId_
        ) =
        execQuery
            [r|
        UPDATE tools
          SET last_returned = ?
        , times_borrowed    = ?
        WHERE tool_id       = ?
    |]
            ( lastReturned_
            , timesBorrowed_
            , toolId_
            )

doAfterReturn :: Int -> IO ()
doAfterReturn toolId' = do
    currentDay <- utctDay <$> getCurrentTime
    tool <- getToolById toolId'
    let updatedTool = newTool <$> tool <*> Just currentDay
    case updatedTool of
        Nothing ->
            putStrLn "No tool was found with that ID"
        Just tool' -> do
            updateTool
                ( LastReturn $ lastReturned tool'
                , TimesBorrowed $ timesBorrowed tool'
                , ToolId toolId'
                )

            putStrLn "Tool updated"
  where
    newTool :: Tool -> Day -> Tool
    newTool tool date =
        tool
            { lastReturned = Just date
            , timesBorrowed = timesBorrowed tool + 1
            }

checkout :: Int -> Int -> IO ()
checkout userId' toolId' = do
    execQuery
        "INSERT INTO checked_out (user_id, tool_id) VALUES (?, ?)"
        (userId', toolId')

checkin :: Int -> IO ()
checkin toolId' = do
    execQuery
        "DELETE FROM checked_out WHERE tool_id = ?"
        (Only toolId')

doCheckin :: Int -> IO ()
doCheckin toolId' = do
    checkin toolId'
    doAfterReturn toolId'