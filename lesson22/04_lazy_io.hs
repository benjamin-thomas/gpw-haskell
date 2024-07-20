{-

In our previous examples, our logic was wrapped up in `IO`, which indicates that
we're not doing a good job of abstracting the overall program.

The root cause is is due to wanting to treat IO data as a sequence of data that have
to be dealt with immediately.

Instead of treating each piece of data as a discrete user action, we can treat the
whole user interaction as a list of characters coming from the user.

 -}

main :: IO ()
main = do
    userInput <- getContents
    mapM_ print userInput