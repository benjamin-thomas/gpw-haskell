{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Char (isPunctuation)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import MyLib (clean, isPalindrome)
import Test.QuickCheck (
    Args (maxSuccess),
    quickCheck,
    quickCheckWith,
    stdArgs,
 )
import Test.QuickCheck.Instances ()

assert :: Bool -> Text -> Text -> IO ()
assert test passMsg failMsg =
    if test
        then TIO.putStrLn passMsg
        else TIO.putStrLn failMsg

prop_punctuationInvariant :: Text -> Bool
prop_punctuationInvariant str = clean str == clean noPunctuationStr
  where
    noPunctuationStr = T.filter (not . isPunctuation) str

prop_reverseInvariant :: Text -> Bool
prop_reverseInvariant str = isPalindrome str == isPalindrome (T.reverse str)

{-

To observe text sent to STDOUT, run like this:

  cabal test --test-show-details=always

---

 -}
main :: IO ()
main = do
    TIO.putStrLn "Running tests..."
    assert (isPalindrome "racecar") "PASS: 'racecar'" "FAIL: 'racecar'"
    assert (isPalindrome "racecar!") "PASS: 'racecar!'" "FAIL: 'racecar!'"
    assert (isPalindrome "racecar.") "PASS: 'racecar.'" "FAIL: 'racecar!'"
    assert (isPalindrome ":racecar:") "PASS: ':racecar:'" "FAIL: 'racecar!'"
    assert (not $ isPalindrome "cat") "PASS: 'cat'" "FAIL: 'cat'"
    quickCheckWith stdArgs{maxSuccess = 1000} prop_punctuationInvariant
    quickCheck prop_reverseInvariant
    TIO.putStrLn "Done!"
