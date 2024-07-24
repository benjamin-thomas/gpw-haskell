import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import System.Environment (getArgs)

import Control.Monad (foldM)
import System.Random (randomRIO)

{-
page 313

runghc ./glitcher.hs lovecraft.jpg

 -}

intToChar :: Int -> Char
intToChar int = toEnum safeInt
 where
  safeInt = int `mod` 255

intToByteString :: Int -> ByteString
intToByteString int = BC.pack [intToChar int]

replaceByte :: Int -> Int -> ByteString -> ByteString
replaceByte loc val bytes = mconcat [before, newVal, after]
 where
  (before, rest) = BC.splitAt loc bytes
  after = BC.drop 1 rest
  newVal = intToByteString val

randomReplaceByte :: ByteString -> IO ByteString
randomReplaceByte bytes = do
  loc <- randomRIO (1, BC.length bytes - 1)
  val <- randomRIO (0, 255)
  return (replaceByte loc val bytes)

sortSection :: Int -> Int -> ByteString -> ByteString
sortSection start size bytes = mconcat [before, changed, after]
 where
  (before, rest) = BC.splitAt start bytes
  (target, after) = BC.splitAt size rest
  changed = BC.reverse (BC.sort target)

randomSortSection :: ByteString -> IO ByteString
randomSortSection bytes = do
  let sectionSize = 25
  let max' = BC.length bytes - sectionSize
  start <- randomRIO (0, max')
  return $ sortSection start sectionSize bytes

glitchActions :: [ByteString -> IO ByteString]
glitchActions =
  [ randomReplaceByte
  , randomSortSection
  , randomReplaceByte
  , randomSortSection
  , randomReplaceByte
  ]

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  imageFile <- BC.readFile fileName
  putStrLn "Read image file"

  glitched <- randomReplaceByte imageFile
  BC.writeFile (mconcat ["glitched_", fileName]) glitched

  glitched2 <- randomSortSection imageFile
  BC.writeFile (mconcat ["glitched2_", fileName]) glitched2

  glitched3 <-
    foldM
      (\bytes f -> f bytes)
      imageFile
      glitchActions
  BC.writeFile (mconcat ["glitched3_", fileName]) glitched3

  print "all done"