import Data.Char (chr)
import System.Random (randomRIO)

{-

>>> map Data.Char.chr [33 .. 126]
"!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"

 -}

randChar :: IO Char
randChar = do
    randPos <- randomRIO (33, 126)
    return $ chr randPos

randChar2 :: IO Char
randChar2 = do
    randPos <- randomRIO (33, 126)
    return $ toEnum randPos

main :: IO ()
main = do
    x1 <- randChar
    x2 <- randChar2
    putStrLn $ "Random char is : " <> show x1
    putStrLn $ "Random char2 is: " <> show x2
