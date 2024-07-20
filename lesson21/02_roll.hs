-- runghc ./lesson21/02_roll.hs

{-

To make System.Random available globally, run this command once before using `runghc`:

cabal install --lib random

 -}
import System.Random (randomRIO)

minDie :: Int
minDie = 1

maxDie :: Int
maxDie = 6

main :: IO ()
main = do
    dieRoll <- randomRIO (minDie, maxDie)
    putStrLn $ "You rolled: " ++ show dieRoll