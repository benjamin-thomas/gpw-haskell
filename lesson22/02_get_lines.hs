{-

runghc ./lesson22/02_get_lines.hs

 -}

{- Quick check 22.1
Write a main that uses `mapM` to call `getLine` three times.
And use `mapM_` to print out the values.
 -}
main :: IO ()
main = do
    values :: [String] <- mapM (const getLine) [(), (), ()] -- or use [1..3]
    mapM_ putStrLn values