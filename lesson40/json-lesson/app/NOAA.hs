{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module NOAA where

import Control.Monad (forM_)
import Data.Aeson (FromJSON (parseJSON), ToJSON, Value (Object), eitherDecode, encode, (.:))
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy.Encoding (decodeLatin1)
import qualified Data.Text.Lazy.IO as TIOL
import GHC.Generics (Generic)

data NOAA_Result = NOAA_Result
    { uid :: Text
    , mindate :: Text
    , maxdate :: Text
    , name :: Text
    , datacoverage :: Float
    , resultId :: T.Text
    }
    deriving (Show)

instance FromJSON NOAA_Result where
    parseJSON (Object v) =
        NOAA_Result
            <$> v .: "uid"
            <*> v .: "mindate"
            <*> v .: "maxdate"
            <*> v .: "name"
            <*> v .: "datacoverage"
            <*> v .: "id"
    parseJSON _ = fail "Expected an object"

data ResultSet = ResultSet
    { offset :: Int
    , count :: Int
    , limit :: Int
    }
    deriving (Show, Generic)

instance FromJSON ResultSet

newtype MetaData = MetaData
    { resultset :: ResultSet
    }
    deriving (Show, Generic)

instance FromJSON MetaData

data NOAA_Response = NOAA_Response
    { metadata :: MetaData
    , results :: [NOAA_Result]
    }
    deriving (Show, Generic)

instance FromJSON NOAA_Response

printResults :: Either String NOAA_Response -> IO ()
printResults (Left err) = do
    putStrLn "Could not decode"
    print err
printResults (Right resp) = do
    putStrLn "Result names:"
    forM_ (results resp) (print . name)

main :: IO ()
main = do
    jsonData <- BSL.readFile "/tmp/tmp.json"
    printResults (eitherDecode jsonData :: Either String NOAA_Response)
    putStrLn "Q40.2"
    let encoded = decodeLatin1 (encode intListExample :: BSL.ByteString)
    TIOL.putStrLn encoded

-- Q40.2

data MyList a
    = EmptyList
    | Cons a (MyList a)
    deriving (Generic)

instance (ToJSON a) => ToJSON (MyList a)

intListExample :: MyList Int
intListExample =
    Cons 1 $ Cons 2 EmptyList
