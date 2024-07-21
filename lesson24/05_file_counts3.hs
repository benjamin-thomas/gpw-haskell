{-# LANGUAGE OverloadedStrings #-}

import Data.List (intercalate)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Environment (getArgs)

{-

To prevent dealing with lazy I/O nastiness, we use the `Text` type instead of `String`.
Since `Text` is non-lazy by default.

  runghc ./05_file_counts3.hs ./stats.dat

 -}

{- |

>>> getCounts "Hello, this is\na test"
(21,5,2)
-}
getCounts :: Text -> (Int, Int, Int)
getCounts input = (charCount, wordCount, lineCount)
 where
  charCount = T.length input
  wordCount = (length . T.words) input
  lineCount = (length . T.lines) input

{- |

>>> countsText (1,2,3)
"chars:1 | words:2 | lines:3"

>>> countsText $ getCounts "Hello, this is\na test"
"chars:21 | words:5 | lines:2"

>>> (countsText . getCounts) "Hello, this is\na test"
"chars:21 | words:5 | lines:2"
-}
countsText :: (Int, Int, Int) -> Text
countsText (cc, wc, lc) =
  let sep = intercalate " | "
   in T.pack $
        sep
          [ "chars:" <> show cc
          , "words:" <> show wc
          , "lines:" <> show lc
          ]

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  input <- TIO.readFile fileName
  let summary = countsText . getCounts $ input
  TIO.putStrLn summary
  TIO.appendFile "stats.dat" (mconcat [T.pack fileName, " ", summary, "\n"])
