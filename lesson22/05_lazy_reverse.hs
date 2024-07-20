{-

Quick check 22.3

Use lazy I/O to write a program that reverses your input and prints it back to you
 -}

main :: IO ()
main = do
    userInput <- getContents
    putStrLn $ reverse userInput