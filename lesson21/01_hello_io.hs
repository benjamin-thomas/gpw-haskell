-- runghc ./lesson21/01_hello_io.hs

greetings :: String -> String
greetings name = "Hello, " ++ name ++ "!"

main :: IO ()
main = do
    putStrLn "Hello! What's your name?"
    name <- getLine
    let greet = greetings name
    putStrLn greet
