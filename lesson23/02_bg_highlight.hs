{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

dharma :: Text
dharma = "धर्म"

bgText :: Text
bgText = "श्रेयान्स्वधर्मो विगुणः परधर्मात्स्वनुष्ठितात्।स्वधर्मे निधनं श्रेयः परधर्मो भयावहः"

highlight :: Text -> Text -> Text
highlight query txt = T.intercalate highlighted pieces
  where
    pieces = T.splitOn query txt
    highlighted = mconcat ["{", query, "}"]

main :: IO ()
main = do
    -- To avoid having to convert back to a `String` for IO, we use TIO instead
    TIO.putStrLn $ highlight dharma bgText