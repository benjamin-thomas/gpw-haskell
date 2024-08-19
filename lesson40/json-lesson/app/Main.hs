{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad (forM_)

import Data.Aeson (FromJSON, ToJSON (toJSON), Value (Object), decode, eitherDecode, encode, object, parseJSON, (.:), (.=))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Function ((&))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import Text.RawString.QQ (r)

{-
Pane 0:
cabal repl

Pane 1:
rg --files | entr bash -c 'tmux send-keys -t gpw-haskell:0 ":reload" C-m ":!clear" C-m "myBook" C-m'
 -}

data Book = Book
    { title :: T.Text
    , author :: T.Text
    , year :: Int
    }
    deriving (Show, Generic)

instance FromJSON Book
instance ToJSON Book

myBook :: Book
myBook =
    Book
        { author = "Will Kurt"
        , title = "Learn Haskell✓"
        , year = 2017
        }

myBookJSON1 :: BSL.ByteString
myBookJSON1 = encode myBook

{-
If I don't call `BLU.fromString`, I will "loose" non-ASCII chars because `pack`
would be called internally via the OverloadedStrings extension (pack would be
called from `Data.ByteString.Lazy.Char8` even though it's not in my current
scope).

Data.ByteString.Lazy.Char8.pack :: [Char]  -> ByteString
Data.ByteString.Lazy.pack       :: [Word8] -> ByteString

And so it's not possible to convert a [Char] to a [Word8].

So in summary: watch out for silent data loss when using the OverloadedStrings extension.

 -}
rawJSON :: BSL.ByteString
-- rawJSON = UTF8.fromString "{\"title\":\"Learn Haskell✓\",\"author\":\"Will Kurt\",\"year\":2017}"
rawJSON = UTF8.fromString [r|{"title":"Learn Haskell✓","author":"Will Kurt","year":2017}|]

decodedBook1 :: Maybe Book
decodedBook1 = decode rawJSON

decodedBook2 :: Either String Book
decodedBook2 = eitherDecode rawJSON

sampleErrorOrig :: BSL.ByteString
sampleErrorOrig = [r|{"message":"something went wrong", "error": 123}|]

data ErrorMessage = ErrorMessage
    { message :: T.Text
    , errorCode :: Int
    }
    deriving (Show)

instance FromJSON ErrorMessage where
    parseJSON (Object v) =
        ErrorMessage
            <$> v .: "message"
            <*> v .: "error"
    parseJSON _ = fail "Expected an object"

instance ToJSON ErrorMessage where
    toJSON (ErrorMessage{message, errorCode}) =
        object
            [ "message" .= message
            , "error" .= errorCode
            ]

main :: IO ()
main = do
    putStrLn "Hello, Haskell!✓"
    putStrLn $ "myBookJSON1: " <> UTF8.toString myBookJSON1
    putStrLn $ "rawJSON: " <> UTF8.toString rawJSON
    putStrLn $ "decodedBook1: " <> show decodedBook1

    -- Below are different ways to access the same decoded data
    putStrLn "\n=> Accessing the (maybe) decoded data..."
    case decodedBook1 of
        Nothing -> pure ()
        Just b -> TIO.putStrLn $ "decodedBook1 title is: " <> title b

    maybe
        (pure ())
        (\b -> TIO.putStrLn $ "decodedBook1 title is: " <> title b)
        decodedBook1

    mapM_
        (\b -> TIO.putStrLn $ "decodedBook1 title is: " <> title b)
        decodedBook1

    decodedBook1 & mapM_ (\b -> TIO.putStrLn $ "decodedBook1 title is: " <> title b)

    forM_ decodedBook1 $
        \b -> TIO.putStrLn $ "decodedBook1 title is: " <> title b

    -- Either
    putStrLn "\n=> Accessing the (either) decoded data..."
    case decodedBook2 of
        Left err -> putStrLn $ "Failed to decoded book2: " <> err
        Right b -> TIO.putStrLn $ "decodedBook2 title is: " <> title b

    either
        (\err -> putStrLn $ "Failed to decoded book2: " <> err)
        (\b -> TIO.putStrLn $ "decodedBook2 title is: " <> title b)
        decodedBook2

    -- We ignore the left (error) branch with these 3 methods
    mapM_
        (\b -> TIO.putStrLn $ "decodedBook2 title is: " <> title b)
        decodedBook2

    decodedBook2 & mapM_ (\b -> TIO.putStrLn $ "decodedBook2 title is: " <> title b)

    forM_ decodedBook2 $
        \b -> TIO.putStrLn $ "decodedBook2 title is: " <> title b

    putStrLn "\n=> Accessing the the sample error..."
    case (eitherDecode sampleErrorOrig :: Either String ErrorMessage) of
        Left err -> putStrLn $ "Failed to decode sampleError: " <> err
        Right sampleErr -> do
            putStrLn $ "decoded sampleError successfully: " <> show sampleErr
            putStrLn "\n=> Re-encoding the sample error back to JSON..."
            putStrLn "Original payload was:"
            putStrLn $ UTF8.toString sampleErrorOrig
            putStrLn "New payload is:"
            putStrLn $ UTF8.toString $ encode sampleErr
