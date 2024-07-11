{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Cipher where

class Codec a where
    encode :: a -> String -> String
    decode :: a -> String -> String

data Rot = Rot

rotHalf :: (Bounded a, Enum a) => Int -> a -> a
rotHalf alphabetSize c = toEnum rotation
  where
    rotation = offset `mod` alphabetSize
    offset = fromEnum c + halfAlphabet
    halfAlphabet = alphabetSize `div` 2

rotCodec :: String -> String
rotCodec = map (rotHalf alphaSize)
  where
    alphaSize = 1 + fromEnum (maxBound :: Char)

instance Codec Rot where
    encode :: Rot -> String -> String
    encode Rot = rotCodec

    decode :: Rot -> String -> String
    decode Rot = rotCodec

{- |

>>> encode Rot "Haskell"
"\557128\557153\557171\557163\557157\557164\557164"

>>> decode Rot $ encode Rot "Haskell"
"Haskell"
-}

-- ============
-- One-time pad
-- ============

type Bits = [Bool]

bitsToInt :: Bits -> Int
bitsToInt bits =
    sum $
        zipWith
            -- 101 (5) is 1*2^2 + 0*2^1 + 1*2^0
            (\exp' bool -> fromEnum bool * 2 ^ exp')
            nats
            (reverse bits)
  where
    nats :: [Integer]
    nats = enumFrom 0

bitsToChar :: Bits -> Char
bitsToChar = toEnum . bitsToInt

charToBits :: Char -> Bits
charToBits = intToBits . fromEnum

maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n =
    if remainder == 0
        then False : intToBits' quotient
        else True : intToBits' quotient
  where
    remainder = n `mod` 2
    quotient = n `div` 2

intToBits :: Int -> Bits
intToBits n = padding ++ leadingBits
  where
    padding = replicate (maxBits - length leadingBits) False
    leadingBits = reverse $ intToBits' n

xor :: [Bool] -> [Bool] -> [Bool]
-- xor xs ys = map xorPair (zip xs ys) -- <== book's version
-- xor = zipWith (curry xorPair)
-- xor = zipWith xorBool
xor = zipWith (/=) -- <== simplified version

applyOTP :: String -> String -> String
applyOTP pad txt =
    map bitsToChar $
        zipWith
            xor
            (map charToBits txt)
            (map charToBits pad)

data OneTimePad = OTP String

instance Codec OneTimePad where
    encode :: OneTimePad -> String -> String
    encode (OTP pad) = applyOTP pad

    decode :: OneTimePad -> String -> String
    decode (OTP pad) = applyOTP pad

myOTP :: OneTimePad
myOTP = OTP (cycle [minBound .. maxBound])

{- |

>>> encode myOTP "Haskell"
"H`qhaij"

>>> encode myOTP "Learn Haskell"
"Ldcqj%Nf{bog`"

>>> decode myOTP $ encode myOTP "Learn Haskell"
"Learn Haskell"

>>> encode myOTP "I can encode text of any length now!"
"I!abj%cikfnn,ykwd1}u4txn8u\DELu{iv?NNU\STX"

>>> encode (OTP $ cycle "xoAQwuMELpXM0PVmtOJc") "Haskell"
"0\SO2:\DC2\EM!"

>>> encode (OTP $ cycle "xoAQwuMELpXM0PVmtOJc") "Haskell programming"
"0\SO2:\DC2\EM!e<\STX7*B1;\NUL\GS!-"

>>> encode (OTP $ cycle "xoAQwuMELpXM0PVmtOJc") "programming Haskell"
"\b\GS.6\ENQ\DC4 (%\RS?mx1%\ACK\DC1#&"

>>> encode (OTP $ cycle "xoA") "Haskell Haskell"
"0\SO2\DC3\n-\DC4O\t\EM\FS*\GS\ETX-"
-}

-- PRNG stands for Pseudo Random Number Generator
prng :: Int -> Int -> Int -> Int -> Int
prng a b maxNumber seed = (a * seed + b) `mod` maxNumber

examplePRNG :: Int -> Int
examplePRNG = prng 1337 7 100

{- |

>>> examplePRNG 0
7

>>> examplePRNG 7
66

>>> examplePRNG 66
49
-}

{-

To explore further, implement a `StreamCipher` type (making it an instance of `Codec`)

I'm not that interested though, so skipping for now.
 -}