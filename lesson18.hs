module Lesson18 where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Tuple (swap)

newtype Box a
    = Box a
    deriving (Show)

{- | Box is a generic type

>>> n = 1
>>> box = Box n
>>> box
Box 1

>>> russianDoll = Box box
>>> russianDoll
Box (Box 1)
-}

-- We can create put things into the box, and take things out of the box
wrap :: a -> Box a
-- wrap x = Box x
wrap = Box

unwrap :: Box a -> a
unwrap (Box x) = x

-- Box is too primitive to be useful. Instead let's create a Triple, which contains 3 values of the same type .
-- Note that this is NOT the same as a Tuple (a,b,c).
data Triple a
    = Triple a a a
    deriving (Show)

type Point3D = Triple Double

aPoint :: Point3D
aPoint =
    Triple
        0.1
        53.2
        12.3

type FullName = Triple String

aPerson :: FullName
aPerson = Triple "Howard" "Phillips" "Lovecraft"

type Initials = Triple Char

initials :: Initials
initials = Triple 'H' 'P' 'L'

-- Now let's create functions, one time, that can work in all of these cases.

first :: Triple a -> a
first (Triple x _ _) = x

second :: Triple a -> a
second (Triple _ x _) = x

third :: Triple a -> a
third (Triple _ _ x) = x

{- |


>>> toList initials
"HPL"

>>> toList aPoint
[0.1,53.2,12.3]
-}
toList :: Triple a -> [a]
toList (Triple x y z) = [x, y, z]

{- |

>>> transform (*2) (Triple 1 2 3)
Triple 2 4 6

>>> transform Data.Char.toLower initials
Triple 'h' 'p' 'l'

>>> toList $ transform Data.Char.toLower initials
"hpl"
-}

-- Note that we don't allow changing the type here (a -> a)
transform :: (a -> a) -> Triple a -> Triple a
transform f (Triple x y z) = Triple (f x) (f y) (f z)

-- Q18.2: implement tripleMap and boxMap

{- |

>>> tripleMap read (Triple "1" "2" "3") :: Triple Int
Triple 1 2 3

>>> boxMap read (Box "1") :: Box Int
Box 1
-}
tripleMap :: (a -> b) -> Triple a -> Triple b
tripleMap f (Triple x y z) = Triple (f x) (f y) (f z)

boxMap :: (a -> b) -> Box a -> Box b
boxMap f (Box x) = Box (f x)

-- Let's reimplement the list type. See `:info []`
data List a
    = Empty
    | Cons a (List a)
    deriving (Show)

(~) :: a -> List a -> List a
(~) = Cons
infixr 5 ~ -- see `:info (:)`
-- I couldn't manage to define Empty with an operator

{- |

>>> 1:2:3:[] :: [Int]
[1,2,3]

>>> 1~2~3~Empty :: List Int
Cons 1 (Cons 2 (Cons 3 Empty))

>>> 'a' : 'b' : 'c' : [] :: [Char]
"abc"

>>> 'a' ~ 'b' ~ 'c' ~ Empty :: List Char
Cons 'a' (Cons 'b' (Cons 'c' Empty))

>>> ourMap (*2) (1 ~ 2 ~ 3 ~ Empty)
Cons 2 (Cons 4 (Cons 6 Empty))
-}
ourMap :: (a -> b) -> List a -> List b
ourMap _ Empty = Empty
ourMap f (Cons x xs) = f x ~ ourMap f xs

-- Kinds: the type of a type as i kind

{- |

A type param returns the final type
>>> :kind []
[] :: * -> *

>>> :kind List
List :: * -> *

The final (concret) type

>>> :kind Int
Int :: *
>>> :kind [Int]
[Int] :: *

>>> :kind List Int
List Int :: *

Requires 2 type params to construct a concret type
>>> :kind (,)
(,) :: * -> * -> *

>>> :kind (Int, Char)
(Int, Char) :: *

>>> :kind Data.Map.Map
Data.Map.Map :: * -> * -> *

>>> :kind Data.Map.Map Int Char
Data.Map.Map Int Char :: *
-}

-- Data.Map (i.e. a Dict, unrelated to the `map` function)
-- To prevent Prelude naming clashes, we use a qualified import

data Organ
    = Heart
    | Brain
    | Kidney
    | Spleen -- la rate
    deriving (Show, Eq, Ord)

organs :: [Organ]
organs =
    [ Heart
    , Heart
    , Brain
    , Spleen
    , Spleen
    , Kidney
    ]

ids :: [Int] -- the drawers
ids = [2, 7, 13, 14, 21, 24]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map Int Organ
organCatalog = Map.fromList organPairs

organInventory :: Map Organ Int -- Q18.2 (add Ord)
organInventory = Map.fromListWith (+) (map swap organPairs)

{- |

>>> organCatalog
fromList [(2,Heart),(7,Heart),(13,Brain),(14,Spleen),(21,Spleen),(24,Kidney)]

>>> Map.lookup 2 organCatalog
Just Heart

>>> Map.lookup 7 organCatalog
Just Heart

>>> Map.lookup 13 organCatalog
Just Brain

>>> Map.lookup 99 organCatalog
Nothing

---

Q18.2

>>> organInventory
fromList [(Heart,9),(Brain,13),(Kidney,24),(Spleen,35)]
-}
