-- runghc ./lesson21/06_fib_io.hs

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

main :: IO ()
main = do
    putStrLn "Which nth Fibonacci number do you want?"
    number <- readLn
    let answer = "Answer: " ++ show (fib number)
    putStrLn answer
