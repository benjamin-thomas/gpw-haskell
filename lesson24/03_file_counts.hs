import Data.List (intercalate)
import System.Environment (getArgs)

{-

Instead of dealing with a `Handle`, we can use other functions with a simpler interface:

- readFile
- writeFile
- appendFile

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
    input <- readFile fileName
    let summary = countsText . getCounts $ input
    appendFile "stats.dat" (mconcat [fileName, " ", summary, "\n"])
    putStrLn summary
