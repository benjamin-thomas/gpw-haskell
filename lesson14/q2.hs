{-# LANGUAGE LambdaCase #-}

-- | Q14.2 declare FiveSidedDie and some related classes
module Lesson14.Q2 where

data FiveSidedDie
    = One
    | Two
    | Three
    | Four
    | Five

class Collection a where
    -- The expected total items. It's a bit dumb, but I don't have better ideas for now.
    totalItems :: a -> Int

class (Collection a) => Die a where
    toInt :: a -> Int

instance Collection FiveSidedDie where
    totalItems :: FiveSidedDie -> Int
    totalItems _ = 5

instance Die FiveSidedDie where
    toInt :: FiveSidedDie -> Int
    toInt = \case
        One -> 1
        Two -> 2
        Three -> 3
        Four -> 4
        Five -> 5

{- |

>>> toInt One
1

>>> toInt Five
5

>>> totalItems One
5

>>> totalItems Two
5
-}
