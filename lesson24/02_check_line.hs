{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use unless" #-}
import Control.Monad (when)
import System.IO (IOMode (ReadMode, WriteMode), hClose, hGetLine, hIsEOF, hPutStrLn, openFile)

main :: IO ()
main = do
    handle <- openFile "./hello.txt" ReadMode
    handle2 <- openFile "./goodbye.txt" WriteMode

    -- Handle line1
    isEOF <- hIsEOF handle
    firstLine <-
        if isEOF
            then return "empty!"
            else
                hGetLine handle
    putStrLn $ "Line1: " <> firstLine

    -- Handle line2
    isEOF2 <- hIsEOF handle
    putStrLn $ "IsEOF2: " <> show isEOF2
    if isEOF2
        then
            return ()
        else do
            line2 <- hGetLine handle
            putStrLn $ "Line2: " <> line2
            hPutStrLn handle2 line2

    -- Handle line3
    isEOF3 <- hIsEOF handle
    putStrLn $ "IsEOF3: " <> show isEOF3
    when (not isEOF3) $ do
        putStrLn "Handling line3"
        line3 <- hGetLine handle
        putStrLn $ "Line3: " <> line3
        hPutStrLn handle2 line3

    hClose handle2
    hClose handle
    putStrLn "Done!"