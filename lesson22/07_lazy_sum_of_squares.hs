{-
Quick check 22.4

Write a program that returns the sum of the squares of the input
 -}

toInts :: String -> [Int]
toInts = map read . lines

main :: IO ()
main = do
    userInput <- getContents
    let numbers = toInts userInput
    let squared = map (^ (2 :: Int)) numbers
    print $ sum squared