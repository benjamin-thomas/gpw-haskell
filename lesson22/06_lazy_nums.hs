toInts :: String -> [Int]
toInts = map read . lines

{-
This is cleaner and more flexible (no need to set the number of lines to read) than `03_get_lines2`
 -}
main :: IO ()
main = do
    userInput <- getContents
    let numbers = toInts userInput
    print $ sum numbers