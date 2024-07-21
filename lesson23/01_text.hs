{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import Data.Text qualified as T

{- |

We may use the more memory efficient `Text` type to represent strings.(~)

`Text` does not use lazy evaluation (unless you import Data.Text.Lazy)
-}

-- We can convert a `String` to a `Text` with `pack`
hello :: Text
hello = T.pack "hello"

-- We can convert a `Text` to a `String` with `unpack`
hello' :: String
hello' = T.unpack hello

-- We can also use the `OverloadedStrings` pragma to "automagically" make the code compile.
-- A lot of `String` functions are also available for the type `Text`.

-- TIP: to assume `Text` by default in GHCi, make sure to call `:set -XOverloadedStrings`

{-

>>> sentences
["hello","world"]
 -}
sentences :: [Text]
sentences = T.lines "hello\nworld"

{- |

>>> input
"hello\nworld\n"
-}
input :: Text
input = T.unlines sentences

{-  |

>>> operations
["A","B","C","D"]

 -}
operations :: [Text]
operations = T.words "A B C D"

{- |

>>> joinedWs
"My name is Benjamin"
-}
joinedWs :: Text
joinedWs = T.unwords ["My", "name", "is", "Benjamin"]

{- |

>>> str
"A,B,C"
-}
str :: Text
str = T.intercalate "," ["A", "B", "C"]

{- |
Since `(++)` operates on lists (and hence on strings), we may use either us `(<>)` or `mconcat`
from `Semigroup` when working with `Text` instead.

Note that those 2 methods would work on the `String` type as well.

>>> joined
"ABC"

>>> joined2
"ABC"

>>> joined3
"ABC"
-}
joined :: Text
joined = "A" <> "B" <> "C"

joined2 :: Text
joined2 = mconcat ["A", "B", "C"]

joined3 :: Text
joined3 = T.concat ["A", "B", "C"]

{- | Quick check 23.3

Create your own version of `T.lines` and `T.unlines` by using `splitOn`
and `T.intercalate`

>>> :set -XOverloadedStrings
>>> T.lines "hello\nworld"
["hello","world"]

>>> lines' "hello\nworld"
["hello","world"]

>>> T.unlines ["A", "B", "C"]
"A\nB\nC\n"

>>> unlines' ["A", "B", "C"]
"A\nB\nC"
-}
lines' :: Text -> [Text]
lines' = T.splitOn "\n"

unlines' :: [Text] -> Text
unlines' = T.intercalate "\n"
