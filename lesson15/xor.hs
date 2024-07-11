module Xor where

xorBool :: Bool -> Bool -> Bool
-- xorBool True True = False
-- xorBool a b = a || b
xorBool = (/=) -- 🤯️

{- |

>>> xorBool True True
False

>>> xorBool False True
True

>>> xorBool True False
True

>>> xorBool False False
False
-}
xorPair :: (Bool, Bool) -> Bool
xorPair (a, b) = xorBool a b

{- |

Here's the basic mechanism of the pipeline
>>> map (\(a,b) -> a + b) $ zip [1,2,3] [4,5,6]
[5,7,9]

Same as:
>>> map (uncurry (+)) $ zip [1,2,3] [4,5,6]
[5,7,9]

Same as:
>>> zipWith (+) [1,2,3] [4,5,6]
[5,7,9]

>>> xor [True, True, False, False] [True, False, True, False]
[False,True,True,False]
-}
xor :: [Bool] -> [Bool] -> [Bool]
-- xor xs ys = map xorPair (zip xs ys) -- <== book's version
-- xor = zipWith (curry xorPair)
-- xor = zipWith xorBool
xor = zipWith (/=) -- <== simplified version

type Bits = [Bool]

{- |

>>> intToBits' 5
[True,False,True]

>>> intToBits' 7
[True,True,True]

>>> intToBits' 8
[False,False,False,True]
-}
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

maxBits :: Int
maxBits = length (intToBits' maxBound)

{- |

>>> Data.List.intercalate "" $ map show $ take 8 $ map fromEnum $ reverse $ intToBits 0
"00000000"

>>> Data.List.intercalate "" $ map show $ take 8 $ map fromEnum $ reverse $ intToBits 1
"10000000"

>>> Data.List.intercalate "" $ map show $ take 8 $ map fromEnum $ reverse $ intToBits 2
"01000000"

>>> Data.List.intercalate "" $ map show $ take 8 $ map fromEnum $ reverse $ intToBits 3
"11000000"

>>> Data.List.intercalate "" $ map show $ take 8 $ map fromEnum $ reverse $ intToBits 4
"00100000"

>>> Data.List.intercalate "" $ map show $ take 8 $ map fromEnum $ reverse $ intToBits 5
"10100000"

>>> Data.List.intercalate "" $ map show $ take 8 $ map fromEnum $ reverse $ intToBits 6
"01100000"

>>> Data.List.intercalate "" $ map show $ take 8 $ map fromEnum $ reverse $ intToBits 7
"11100000"

>>> Data.List.intercalate "" $ map show $ take 8 $ map fromEnum $ reverse $ intToBits 8
"00010000"
-}
intToBits :: Int -> Bits
intToBits n = padding ++ leadingBits
  where
    padding = replicate (maxBits - length leadingBits) False
    leadingBits = reverse $ intToBits' n

{- |

>>> Data.List.intercalate "" $ map show $ take 8 $ map fromEnum $ reverse $ intToBits 97
"10000110"

>>> Data.List.intercalate "" $ map show $ take 8 $ map fromEnum $ reverse $ charToBits 'a'
"10000110"
-}
charToBits :: Char -> Bits
charToBits = intToBits . fromEnum

{- |


>>> sum $ map (\(pow,bool) -> (fromEnum bool)*2^pow) $ zip [0..] (reverse $ intToBits 8)
8

>>> bitsToInt $ intToBits 0
0

>>> bitsToInt $ intToBits 1
1

>>> bitsToInt $ intToBits 2
2

>>> bitsToInt $ intToBits 3
3

>>> bitsToInt $ intToBits 4
4

>>> bitsToInt $ intToBits 5
5

>>> bitsToInt $ intToBits 6
6

>>> bitsToInt $ intToBits 7
7

>>> bitsToInt $ intToBits 8
8

>>> (bitsToInt $ intToBits maxBound) == (maxBound :: Int)
True
-}
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

{- |

>>> bitsToChar $ intToBits 97
'a'

>>> bitsToChar $ charToBits 'a'
'a'

>>> bitsToChar $ charToBits maxBound
'\1114111'

>>> bitsToChar $ charToBits minBound
'\NUL'
-}
bitsToChar :: Bits -> Char
bitsToChar = toEnum . bitsToInt

-- Applying a one-time pad

-- | NOTE: the pad needs to be as long as the text to be encrypted
myPad :: String
myPad = "Shhhhhh"

myTxt :: String
myTxt = "Haskell"

{- |

>>> applyOTP myPad myTxt
"\ESC\t\ESC\ETX\r\EOT\EOT"
-}
applyOTP :: String -> String -> String
applyOTP pad txt =
    map bitsToChar $
        zipWith
            xor
            (map charToBits txt)
            (map charToBits pad)

{- |

>>> codec "Haskell"
"\ESC\t\ESC\ETX\r\EOT\EOT"

>>> codec $ codec "Haskell"
"Haskell"

The codec works up to the pad's length
>>> codec $ codec "My name is Benjamin"
"My name"
-}
codec :: String -> String
codec = applyOTP myPad
