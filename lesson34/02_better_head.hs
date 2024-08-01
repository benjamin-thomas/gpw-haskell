import Prelude hiding (head)

{-

Some LISPs return nil when requesting head from an empty list

\$ clojure
Clojure 1.10.2
(rest '(1 2 3))
(2 3)
(first '(1 2 3))
1
(first '(1))
1
(first '())
nil

Let's say we wanted to replicate this behavior.
We cannot just return `[]` because we go from `[a]` to `a`.
So that would be a type error.

Instead, we can require the input to be an instance of `Monoid`, so that we can
return the empty representation.
 -}

{- | For this to work, the item type must be an instance of Monoid

>>> head (["Hello", "World"]:: [String])
"Hello"

>>> head ([]:: [String])
""

>>> Prelude.head ([]:: [String])
Prelude.head: empty list

>>> head ([3] :: [Data.Monoid.Product Int]) <> 4
Product {getProduct = 12}

>>> head ([] :: [Data.Monoid.Product Int]) <> 4
Product {getProduct = 4}

>>> head ([] :: [Data.Monoid.Product Int])
Product {getProduct = 1}

>>> Prelude.head ([] :: [Data.Monoid.Product Int])
Prelude.head: empty list
-}
head :: (Monoid a) => [a] -> a
head [] = mempty
head (x : _) = x

-- >>> head [1,2]
-- Variable not in scope: head :: [a0_agxV[tau:1]] -> t_agxS[sk:1]
