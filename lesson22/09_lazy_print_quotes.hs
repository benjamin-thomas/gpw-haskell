{-

Q22.2

The user should continuously select a number between 1 and 5 to print a quote.
If the user enter "n", the program exits.

Use lazy IO, rather than recursively calling main

doctest ./lesson22/09_lazy_print_quotes.hs
runghc ./lesson22/09_lazy_print_quotes.hs
 -}
{-# LANGUAGE LambdaCase #-}

newtype Quote = Quote String

quotes :: (Quote, Quote, Quote, Quote, Quote)
quotes =
    ( Quote "Quote A"
    , Quote "Quote B"
    , Quote "Quote C"
    , Quote "Quote D"
    , Quote "Quote E"
    )

quote1 :: Quote
quote1 = (\(a, _, _, _, _) -> a) quotes

quote2 :: Quote
quote2 = (\(_, b, _, _, _) -> b) quotes

quote3 :: Quote
quote3 = (\(_, _, c, _, _) -> c) quotes

quote4 :: Quote
quote4 = (\(_, _, _, d, _) -> d) quotes

quote5 :: Quote
quote5 = (\(_, _, _, _, e) -> e) quotes

toQuote :: [String] -> [Quote]
toQuote = \case
    [] -> []
    "n" : _ -> []
    "1" : rest -> quote1 : toQuote rest
    "2" : rest -> quote2 : toQuote rest
    "3" : rest -> quote3 : toQuote rest
    "4" : rest -> quote4 : toQuote rest
    "5" : rest -> quote5 : toQuote rest
    x : _ -> error $ "toQuote: bad input => " ++ show x

toString :: Quote -> String
toString (Quote s) = s

{- |

>>> map toString $ toQuote ["1", "1", "3", "n", "4"]
["Quote A","Quote A","Quote C"]
-}
main :: IO ()
main = do
    contents <- getContents
    let commands :: [String] = lines contents
    let output :: [String] = map toString (toQuote commands)
    mapM_ print output
