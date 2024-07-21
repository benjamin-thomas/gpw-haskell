import Data.List (intercalate)
import System.Environment (getArgs)
import System.IO (IOMode (..), hClose, hGetContents, openFile)

{-

The previous program can't handle generating stats on the stats file itself.

  runghc ./04_file_counts2.hs ./stats.dat

That's because `readFile` does not close the underlying `Handle`.

 -}

{- |

>>> getCounts "Hello, this is\na test"
(21,5,2)
-}
getCounts :: String -> (Int, Int, Int)
getCounts input = (charCount, wordCount, lineCount)
 where
  charCount = length input
  wordCount = (length . words) input
  lineCount = (length . lines) input

{- |

>>> countsText (1,2,3)
"chars:1 | words:2 | lines:3"

>>> countsText $ getCounts "Hello, this is\na test"
"chars:21 | words:5 | lines:2"

>>> (countsText . getCounts) "Hello, this is\na test"
"chars:21 | words:5 | lines:2"
-}
countsText :: (Int, Int, Int) -> String
countsText (cc, wc, lc) =
  let sep = intercalate " | "
   in sep
        [ "chars:" <> show cc
        , "words:" <> show wc
        , "lines:" <> show lc
        ]

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  handle <- openFile fileName ReadMode
  input <- hGetContents handle
  let summary = countsText . getCounts $ input
  -- Lazy I/O is nasty! Commenting out the putStrLn below will generate a runtime error!
  putStrLn summary
  hClose handle
  appendFile "stats.dat" (mconcat [fileName, " ", summary, "\n"])
