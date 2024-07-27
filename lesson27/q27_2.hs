newtype Box a = Box a deriving (Show)

instance Functor Box where
    fmap :: (a -> b) -> Box a -> Box b
    fmap f (Box v) = Box (f v)

myBox :: Box Int
myBox = Box 1

otherBox :: Box Int
otherBox = Box 10

wrapped :: Box (Box Int)
wrapped = fmap Box myBox

unwrap :: Box a -> a
unwrap (Box v) = v

{-

>>> myBox
Box 1
>>> otherBox
Box 10

>>> wrapped
Box (Box 1)

>>> unwrap wrapped
Box 1

>>> fmap unwrap wrapped
Box 1

-}
