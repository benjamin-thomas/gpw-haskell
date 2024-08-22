-- cabal install --lib array

import Control.Monad (foldM_, forM_, when, (<=<), (>=>))
import Control.Monad.ST (ST, runST)

import Data.Array.ST (
    MArray (newArray),
    STUArray,
    readArray,
    runSTUArray,
    thaw,
    writeArray,
 )

import Data.Array.Unboxed (UArray, accum, array, bounds, listArray, (!), (//))
import Data.STRef (newSTRef, readSTRef, writeSTRef)

{-
ghci> :set +s
ghci> aLargeList !! 9_999_999
10000000
(0.83 secs, 720,086,232 bytes)
 -}
aLargeList :: [Int]
aLargeList = [1 .. 10_000_000]

{-
ghci> aLargeArray ! 9_999_999
0
(0.06 secs, 80,078,024 bytes)
 -}
aLargeArray :: UArray Int Int
aLargeArray = array (0, 9_999_999) []

-- >>> lastItem
-- 0
lastItem :: Int
lastItem = aLargeArray ! 9_999_999

{-
ghci> length aLargeListDoubled
10000000
(1.44 secs, 1,600,086,384 bytes)
ghci> length aLargeListDoubled
10000000
(0.11 secs, 82,968 bytes)
 -}
aLargeListDoubled :: [Int]
aLargeListDoubled = map (* 2) aLargeList

{-
Unless the kv is set, arrays are initialized to their default values
>>> zeroIndexArray
array (0,5) [(0,False),(1,False),(2,False),(3,True),(4,False),(5,False)]

>>> zeroIndexArray ! 5
False

>>> zeroIndexArray ! 3
True

>>> zeroIndexArray ! 0
False
 -}
zeroIndexArray :: UArray Int Bool
zeroIndexArray = array (0, 5) [(3, True)]

{-
>>> oneIndexArray
array (1,6) [(1,True),(2,True),(3,True),(4,True),(5,True),(6,True)]

>>> oneIndexArray ! 1
True

>>> oneIndexArray ! 6
True

>>> oneIndexArray ! 0
Ix{Int}.index: Index (0) out of range ((1,6))
 -}
oneIndexArray :: UArray Int Bool
oneIndexArray = array (1, 6) $ map (,True) [1 .. 6]

-- Quick check 42.1
-- >>> qcArray
-- array (0,2) [(0,True),(1,False),(2,False)]
qcArray :: UArray Int Bool
qcArray = array (0, 2) [(0, True)]

-- UPDATING THE ARRAY

{-
>>> beansInBucket
array (0,3) [(0,0),(1,0),(2,0),(3,0)]

We can set new values like such:
>>> beansInBucket // [(1,9), (3,8)]
array (0,3) [(0,0),(1,9),(2,0),(3,8)]

 -}
beansInBucket :: UArray Int Int
beansInBucket = array (0, 3) []

{-
Quick check 42.2

>>> beansInBucket2 0
array (0,3) [(0,0),(1,0),(2,0),(3,0)]

>>> beansInBucket2 9
array (0,3) [(0,9),(1,9),(2,9),(3,9)]
 -}
beansInBucket2 :: Int -> UArray Int Int
beansInBucket2 default' = array (0, 3) $ map (,default') [0 .. 3]

{-
>>> beansAdd2
array (0,3) [(0,9),(1,2),(2,2),(3,2)]

 -}
beansAdd2 :: UArray Int Int
beansAdd2 =
    accum (+) (beansInBucket // [(0, 7)]) $ map (,2) [0 .. 3]

{- Quick check 42.3
>>> beansMul2
array (0,3) [(0,18),(1,4),(2,4),(3,4)]

 -}
beansMul2 :: UArray Int Int
beansMul2 = accum (*) beansAdd2 $ map (,2) [0 .. 3]

-- == Using the type ST ==

-- >>> runSTUArray $ listToStUArray [1,3,2]
-- array (0,2) [(0,1),(1,3),(2,2)]
listToStUArray :: [Int] -> ST s (STUArray s Int Int)
listToStUArray vals = do
    let end = length vals - 1
    myArray <- newArray (0, end) 0
    forM_ [0 .. end] $ \i -> do
        let val = vals !! i
        writeArray myArray i val
    return myArray

-- Two variants below that are more efficient since we don't index into the list
-- >>> runSTUArray $ listToStUArray2 [1,3,2]
-- array (0,2) [(0,1),(1,3),(2,2)]
listToStUArray2 :: [Int] -> ST s (STUArray s Int Int)
listToStUArray2 vals = do
    let end = length vals - 1
    myArray <- newArray (0, end) 0

    -- Traverse the list once and populate the array
    let fillArray _ [] = return ()
        fillArray i (x : xs) = do
            writeArray myArray i x
            fillArray (i + 1) xs

    fillArray 0 vals
    return myArray

-- >>> runSTUArray $ listToStUArray3 [1,3,2]
-- array (0,2) [(0,1),(1,3),(2,2)]
listToStUArray3 :: [Int] -> ST s (STUArray s Int Int)
listToStUArray3 vals = do
    let end = length vals - 1
    myArray <- newArray (0, end) 0

    -- Traverse the list once and populate the array
    foldM_
        (\i val -> writeArray myArray i val >> return (i + 1))
        0
        vals
    return myArray

-- >>> runSTUArray $ listToStUArray4 [1,3,2]
-- array (0,2) [(0,1),(1,3),(2,2)]
listToStUArray4 :: [Int] -> ST s (STUArray s Int Int)
listToStUArray4 vals = do
    let end = length vals - 1
    myArray <- newArray (0, end) 0

    -- Traverse the list once and populate the array
    foldM_
        (\i -> const (return (i + 1)) <=< writeArray myArray i)
        0
        vals
    return myArray

-- >>> runSTUArray $ listToStUArray5 [1,3,2]
-- array (0,2) [(0,1),(1,3),(2,2)]
listToStUArray5 :: [Int] -> ST s (STUArray s Int Int)
listToStUArray5 vals = do
    let end = length vals - 1
    myArray <- newArray (0, end) 0

    -- Traverse the list once and populate the array
    foldM_
        -- (\i val -> writeArray myArray i val >> return (i + 1))
        (\i -> writeArray myArray i >=> const (return $ i + 1))
        0
        vals
    return myArray

-- To extract the array out ou the ST context, we use runSTUArray
listToUArray :: [Int] -> UArray Int Int
listToUArray vals = runSTUArray $ listToStUArray vals

{-
Where as STUArray used :

- newArray
- readArray
- writeArray

We can use the more general ST type itself, which implements:

- newSTRef
- readSTRef
- writeSTRef

---

Here, we swap a tuple in a stateful manner:

>>> swapST (1,2)
(2,1)
 -}
swapST :: (Int, Int) -> (Int, Int)
swapST (x, y) = runST $ do
    x1 <- newSTRef x
    y1 <- newSTRef y
    writeSTRef y1 x
    writeSTRef x1 y
    x2 <- readSTRef x1
    y2 <- readSTRef y1
    return (x2, y2)

-- === BUBBLE SORT ===

{- First we use the std lib function (almost equivalent to the one we defined above)
>>> myData
array (0,5) [(0,7),(1,6),(2,4),(3,8),(4,10),(5,2)]
 -}
myData :: UArray Int Int
myData = listArray (0, 5) [7, 6, 4, 8, 10, 2]

{-
>>> bubbleSort myData
array (0,5) [(0,2),(1,4),(2,6),(3,7),(4,8),(5,10)]
 -}
bubbleSort :: UArray Int Int -> UArray Int Int
bubbleSort myArray = runSTUArray $ do
    stArray <- thaw myArray
    let end = (snd . bounds) myArray
    forM_ [1 .. end] $ \i -> do
        forM_ [0 .. (end - i)] $ \j -> do
            val <- readArray stArray j
            nextVal <- readArray stArray (j + 1)
            when (val > nextVal) $ do
                writeArray stArray j nextVal
                writeArray stArray (j + 1) val
    return stArray

{-

void bubbleSort(int arr[], int len) {
  for (int i = 0; i < len - 1; i++) {
    for (int j = 0; j < len - i - 1; j++) {
      if (arr[j] > arr[j + 1]) {
        int temp = arr[j];
        arr[j] = arr[j + 1];
        arr[j + 1] = temp;
      }
    }
  }
}

>>> bubbleSort2 myData
array (0,5) [(0,2),(1,4),(2,6),(3,7),(4,8),(5,10)]

 -}
bubbleSort2 :: UArray Int Int -> UArray Int Int
bubbleSort2 myArray = runSTUArray $ do
    stArray <- thaw myArray
    let len = (snd . bounds) myArray
    forM_ [0 .. (len - 1)] $ \i -> do
        forM_ [0 .. (len - i - 1)] $ \j -> do
            a <- readArray stArray j
            b <- readArray stArray (j + 1)
            when (a > b) $ do
                writeArray stArray j b
                writeArray stArray (j + 1) a
    return stArray

{-
>>> bubbleSort3 myData
array (0,5) [(0,2),(1,4),(2,6),(3,7),(4,8),(5,10)]
 -}
bubbleSort3 :: UArray Int Int -> UArray Int Int
bubbleSort3 myArray = runSTUArray $ do
    stArray <- thaw myArray
    let len = (snd . bounds) myArray
    foldM_
        ( \_ i ->
            foldM_
                ( \_ j -> do
                    a <- readArray stArray j
                    b <- readArray stArray (j + 1)
                    when (a > b) $ do
                        writeArray stArray j b
                        writeArray stArray (j + 1) a
                )
                ()
                [0 .. (len - i - 1)]
        )
        ()
        [0 .. (len - 1)]
    return stArray

{-
>>> bubbleSort4 myData
array (0,5) [(0,2),(1,4),(2,6),(3,7),(4,8),(5,10)]
 -}
bubbleSort4 :: UArray Int Int -> UArray Int Int
bubbleSort4 myArray = runSTUArray $ do
    stArray <- thaw myArray
    let len = (snd . bounds) myArray

    mapM_
        ( \i ->
            mapM_
                ( \j -> do
                    a <- readArray stArray j
                    b <- readArray stArray (j + 1)
                    when (a > b) $ do
                        writeArray stArray j b
                        writeArray stArray (j + 1) a
                )
                [0 .. (len - i - 1)]
        )
        [0 .. (len - 1)]

    return stArray

{-
Q42.1

>>> crossover 3 (listArray (0,4) (repeat 1), listArray (0,4) (repeat 0))
array (0,4) [(0,1),(1,1),(2,1),(3,0),(4,0)]
 -}

crossover :: Int -> (UArray Int Int, UArray Int Int) -> UArray Int Int
crossover cutoff (ones, zeros) = runSTUArray $ do
    stArray <- thaw ones
    let len = (snd . bounds) ones
    forM_ [0 .. len] $ \i -> do
        when (i >= cutoff) $ do
            let z = zeros ! i
            writeArray stArray i z

    return stArray

{- Q42.2

>>> replaceZeros (listArray (0,4) [1,0,8,2,0])
array (0,4) [(0,1),(1,-1),(2,8),(3,2),(4,-1)]
 -}
replaceZeros :: UArray Int Int -> UArray Int Int
replaceZeros arr = runSTUArray $ do
    let len = (snd . bounds) arr
    stArr <- thaw arr
    forM_ [0 .. len] $ \i -> do
        when ((arr ! i) == 0) $ do
            writeArray stArr i (-1)
    return stArr
