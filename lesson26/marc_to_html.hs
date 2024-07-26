{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Bifunctor (bimap, first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO qualified as TIO

type Author = Text
type Title = Text

data Book = Book
    { author :: Author
    , title :: Title
    }
    deriving (Show)

type Html = Text

{-

>>> bookToHtml (Book "J. K. Rowling" "Harry Potter and the Chamber of Secrets")
"<p><strong>Harry Potter and the Chamber of Secrets</strong><em>J. K. Rowling</em></p>"

 -}
bookToHtml :: Book -> Html
bookToHtml book =
    mconcat
        [ "<li>"
        , mconcat ["<strong>", title book, "</strong>"]
        , mconcat ["&nbsp;", ":", "&nbsp;"]
        , mconcat ["<em>", author book, "</em>"]
        , "</li>"
        ]

booksToHtml :: [Book] -> Html
booksToHtml books =
    mconcat
        [ "<html>"
        , mconcat
            [ "<head>"
            , "<title>Books</title>"
            , "<meta charset='utf-8' />"
            , "</head>"
            ]
        , "<body>"
        , "<ol>"
        , mconcat (map bookToHtml books)
        , "</ol>"
        , "</body>"
        , "</html>"
        ]

book1 :: Book
book1 =
    Book
        { title = "The Conspiracy Against the Human Race"
        , author = "Ligotti, Thomas"
        }
book2 :: Book
book2 =
    Book
        { title = "A Short History of Decay"
        , author = "Cioran, Emil"
        }
book3 :: Book
book3 =
    Book
        { title = "The Tears of Eros"
        , author = "Bataille, Georges"
        }

allBooks :: [Book]
allBooks = [book1, book2, book3]

type MarcRecordRaw = ByteString
type MarcLeaderRaw = ByteString

leaderLength :: Int
leaderLength = 24

getLeader :: MarcRecordRaw -> MarcLeaderRaw
getLeader = B.take leaderLength

rawToInt :: ByteString -> Int
rawToInt = read . T.unpack . decodeUtf8

getRecordLength :: MarcLeaderRaw -> Int
getRecordLength = rawToInt . B.take 5

{- | Now that we've figured out how to extract the length of a single MARC record,
we will devise a function to takes a ByteString and returns ONE record followed by
the rest of the ByteString
-}
nextAndRest :: ByteString -> (MarcRecordRaw, ByteString)
nextAndRest bs = B.splitAt (getRecordLength bs) bs

allRecords :: ByteString -> [MarcRecordRaw]
allRecords bs =
    if B.empty == bs
        then
            []
        else
            let (curr, rest) = nextAndRest bs
             in curr : allRecords rest

type MarcDirectoryRaw = ByteString

getBaseAddress :: MarcLeaderRaw -> Int
getBaseAddress = rawToInt . B.take 5 . B.drop 12

getDirectoryLength :: MarcLeaderRaw -> Int
getDirectoryLength leader = getBaseAddress leader - (leaderLength + 1)

getDirectory :: MarcRecordRaw -> MarcDirectoryRaw
getDirectory record =
    B.take directoryLength afterLeader
  where
    directoryLength = getDirectoryLength record
    afterLeader = B.drop leaderLength record

type MarcDirectoryEntryRaw = ByteString

-- Each field metadata has a size of 12 bytes
dirEntryLength :: Int
dirEntryLength = 12

splitDirectory :: MarcDirectoryRaw -> [MarcDirectoryEntryRaw]
splitDirectory directory =
    let
        -- (entry, rest) = (B.take dirEntryLength directory, B.drop dirEntryLength directory)
        (entry, rest) = B.splitAt dirEntryLength directory
     in
        if B.empty == rest then [] else entry : splitDirectory rest

data FieldMetaData = FieldMetaData
    { fieldTag :: Text
    , fieldLength :: Int
    , fieldStart :: Int
    }
    deriving (Show)

makeFieldMetaData :: MarcDirectoryEntryRaw -> FieldMetaData
makeFieldMetaData entry =
    {-
        (-1) to remove trailing GS/RS (group separator/record separator)
        It's probably not quite the way to go (I "fixed" this then saw the
        mentioning of this problem later on, at the end of the lesson)

     -}
    FieldMetaData tag (len - 1) start
  where
    (tag, rest) = first decodeUtf8 $ B.splitAt 3 entry
    (len, start) = bimap rawToInt rawToInt $ B.splitAt 4 rest

getFieldMetaData :: [MarcDirectoryEntryRaw] -> [FieldMetaData]
getFieldMetaData = map makeFieldMetaData

type FieldText = Text

getTextField :: MarcRecordRaw -> FieldMetaData -> FieldText
getTextField record metaData =
    let
        baseAddress = getBaseAddress record
        baseRecord = B.drop baseAddress record
        baseAtEntry = B.drop (fieldStart metaData) baseRecord
        bytes = B.take (fieldLength metaData) baseAtEntry
     in
        decodeUtf8 bytes

fieldDelimiter :: Char
fieldDelimiter = toEnum 31

titleTag :: Text
titleTag = "245"

titleSubField :: Char
titleSubField = 'a'

authorTag :: Text
authorTag = "100"

authorSubField :: Char
authorSubField = 'a'

lookupFieldMetaData :: Text -> MarcRecordRaw -> Maybe FieldMetaData
lookupFieldMetaData tag record =
    let metaData = (getFieldMetaData . splitDirectory . getDirectory) record
        items = filter ((== tag) . fieldTag) metaData
     in listToMaybe items

lookupSubField :: Maybe FieldMetaData -> Char -> MarcRecordRaw -> Maybe Text
lookupSubField Nothing _ _ = Nothing
lookupSubField (Just metaData) subField record =
    let
        rawField = getTextField record metaData
        subFields = T.split (== fieldDelimiter) rawField
        items = map (T.drop 1) $ filter ((== subField) . T.head) subFields -- WARNING: T.head is partial
     in
        listToMaybe items

lookupValue :: Text -> Char -> MarcRecordRaw -> Maybe Text
lookupValue tag subField record =
    let entryMetaData = lookupFieldMetaData tag record
     in lookupSubField entryMetaData subField record

lookupTitle :: MarcRecordRaw -> Maybe Title
lookupTitle = lookupValue titleTag titleSubField

lookupAuthor :: MarcRecordRaw -> Maybe Author
lookupAuthor = lookupValue authorTag authorSubField

-- marcToPairs :: ByteString -> [(Maybe Title, Maybe Author)]
-- marcToPairs bytes =
--     map
--         (\record -> (lookupTitle record, lookupAuthor record))
--         (allRecords bytes)

marcToPairs :: ByteString -> [(Maybe Title, Maybe Author)]
marcToPairs =
    map toPair . allRecords
  where
    toPair record = (lookupTitle record, lookupAuthor record)

pairsToBooks :: [(Maybe Title, Maybe Author)] -> [Book]
pairsToBooks = mapMaybe toBook
  where
    toBook (Just title, Just author) = Just (Book title author)
    toBook _ = Nothing

-- processRecords :: Int -> ByteString -> Html
-- processRecords n marcData =
--     booksToHtml books
--   where
--     books = pairsToBooks (take n pairs)
--     pairs = marcToPairs marcData

processRecords :: Int -> ByteString -> Html
processRecords n = booksToHtml . take n . pairsToBooks . marcToPairs

main :: IO ()
main = do
    marcData <- B.readFile "./sample.mrc"
    let marcRecords = allRecords marcData
    -- putStrLn $
    --     mconcat
    --         [ "Found "
    --         , show $ length marcRecords
    --         , " records!"
    --         ]
    TIO.putStrLn $
        mconcat
            [ "Found "
            , T.pack $ show $ length marcRecords
            , " records!"
            ]
    TIO.writeFile "books.html" (booksToHtml allBooks)
    let html = processRecords 20 marcData
    TIO.writeFile "books2.html" html
