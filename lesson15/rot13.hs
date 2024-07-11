module Rot13 where

data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded)

rotHalf :: (Bounded a, Enum a) => Int -> a -> a
rotHalf alphabetSize c = toEnum rotation
 where
  rotation = offset `mod` alphabetSize
  offset = fromEnum c + halfAlphabet
  halfAlphabet = alphabetSize `div` 2

{- |


>>> rotHalf 4 L1
L3

>>> rotHalf 4 L2
L4

>>> rotHalf 4 L3
L1

>>> rotHalf 4 L4
L2

>>> rotHalf 2 True
False

>>> rotHalf 2 False
True
-}
rotChar :: Char -> Char
rotChar = rotHalf alphaSize
 where
  alphaSize = 1 + fromEnum (maxBound :: Char)

{- |

?? The book talks about rot13 but that's got nothing to do with rot13 per se...
>>> rotChar 'a'
'\557153'
-}
fourLetterAlphaEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterAlphaEncoder =
  map (rotHalf alphaSize)
 where
  alphaSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)

{- |

>>> fourLetterAlphaEncoder [L1, L2, L3, L4]
[L3,L4,L1,L2]

>>> fourLetterAlphaEncoder [L1, L3, L4, L1, L1, L2]
[L3,L1,L2,L3,L3,L4]

>>> fourLetterAlphaEncoder [L3,L1,L2,L3,L3,L4]
[L1,L3,L4,L1,L1,L2]
-}

-- The rest of the rot13 related lesson isn't very interesting...