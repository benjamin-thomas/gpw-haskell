import Data.ByteString qualified as B
import Data.Text.Encoding (decodeUtf8)

import Data.Text qualified as T

{-

Q25.1 Write a program that reads in a text file and outputs the difference between the
number of characters in the file and the number of bytes in the file.

 -}

main :: IO ()
main = do
    bytes <- B.readFile "/usr/share/dict/words"
    let bytesCount = B.length bytes
    let charsCount = T.length $ decodeUtf8 bytes
    putStrLn $ "Bytes count: " <> show bytesCount
    putStrLn $ "Chars count: " <> show charsCount
    putStrLn $ "Difference: " <> show (bytesCount - charsCount)

{- Q25.2 Not interesting -}