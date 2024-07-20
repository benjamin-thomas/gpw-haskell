import Control.Monad (replicateM)
import System.Environment (getArgs)

{-

runghc ./lesson22/03_get_lines2.hs 3

 -}

{- Use a command-line argument to determine how many lines to read
 -}
main :: IO ()
main = do
    args <- getArgs
    let linesToRead :: Int = if not (null args) then read $ head args else 0
    putStrLn $ "Enter " ++ show linesToRead ++ " numbers:"
    -- `replicateM` repeats x times a given action (in a monadic context)
    numbers :: [Int] <- replicateM linesToRead readLn
    putStrLn $ "Enter " ++ show linesToRead ++ " more numbers:"
    moreNumbers :: [Int] <- replicateM' linesToRead readLn
    print $ sum numbers + sum moreNumbers

{-
Quick check 22.2
Write your version of `replicateM` with `mapM`
 -}
replicateM' :: (Monad m, Num a, Enum a) => a -> m b -> m [b]
replicateM' n f = mapM (const f) [1 .. n]