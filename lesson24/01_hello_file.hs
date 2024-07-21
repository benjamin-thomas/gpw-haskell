import System.IO

main :: IO ()
main = do
    handle <- openFile "hello.txt" ReadMode
    firstLine <- hGetLine handle
    putStrLn firstLine
    secondLine <- hGetLine handle
    handle2 <- openFile "goodbye.txt" WriteMode -- the file may or may not exist, it's okay.
    hPutStrLn handle2 secondLine
    hClose handle2
    hClose handle
    putStrLn "Done3"