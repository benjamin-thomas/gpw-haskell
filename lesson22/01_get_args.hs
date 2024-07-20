import System.Environment (getArgs)

{-

[gpw-haskell]$ runghc ./lesson22/01_get_args.hs hello world
hello
world

 -}

main :: IO ()
main = do
    args <- getArgs
    -- `mapM` allows to map an IO action (or more specifically a function in the Monad context),
    -- over over a list of values
    -- `mapM_` is the same as `mapM` but discards the result
    mapM_ putStrLn args
