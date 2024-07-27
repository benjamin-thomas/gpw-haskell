{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import Data.Word (Word8)

{-

`ByteString` represents bytes, which could themselves be represented with `Char`s since there
are 256 ASCII characters (2^8 or 8 bits).

Thusly, we can use the OverLoadedStrings extension to represent byte streams with chars.

 -}

sampleBytes :: ByteString
sampleBytes = "Hello!"

{-

ghci> :info B.unpack
B.unpack :: ByteString -> [GHC.Word.Word8]

ghci> :info BC.unpack
BC.unpack :: ByteString -> [Char]
        -- Defined in ‘Data.ByteString.Char8’

>>> sampleBytes
"Hello!"

>>> bytesArray
[72,101,108,108,111,33]

>>> sampleString
"Hello!"
 -}

bytesArray :: [Word8]
bytesArray = B.unpack sampleBytes

sampleString :: String
sampleString = BC.unpack sampleBytes

{-
Quick check 25.1

Write a function that takes a number in ASCII character form and converts the to ints

 -}

bcInt :: ByteString -> Int
bcInt = read . BC.unpack
