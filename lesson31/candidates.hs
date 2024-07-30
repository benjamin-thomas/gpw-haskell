{-

31.2 Using do-notation to reuse the same code in different
contexts

 -}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Data.Map (Map)
import Data.Map qualified as Map

{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Use <$>" #-}

data Grade = F | D | C | B | A deriving (Eq, Ord, Enum, Show, Read)
data Degree = HS | BA | MS | PhD deriving (Eq, Ord, Enum, Show, Read)

data Candidate = Candidate
    { candidateId :: Int
    , codeReview :: Grade
    , cultureFit :: Grade
    , education :: Degree
    }
    deriving (Show)

viable :: Candidate -> Bool
viable candidate =
    and tests
  where
    tests = [passedCoding, passedCultureFit, educationMin]
    passedCoding = codeReview candidate > B
    passedCultureFit = cultureFit candidate > C
    educationMin = education candidate >= MS

readInt :: IO Int
-- readInt = getLine >>= (return . read)
readInt = read <$> getLine

readGrade :: IO Grade
readGrade = read <$> getLine

-- Quick check 31.3 Rewrite readGrade with do-notation.
readGrade' :: IO Grade
readGrade' = do
    line <- getLine
    return (read line)

readDegree :: IO Degree
readDegree = read <$> getLine

readCandidate :: IO Candidate
readCandidate = do
    putStrLn "Enter id:"
    candidateId <- readInt

    putStrLn "Enter code grade:"
    codeReview <- readGrade

    putStrLn "Enter culture fit grade:"
    cultureFit <- readGrade

    putStrLn "Enter education:"
    education <- readDegree

    return $
        Candidate
            { candidateId = candidateId
            , codeReview = codeReview
            , cultureFit = cultureFit
            , education = education
            }

assessCandidateIO :: IO String
assessCandidateIO = do
    candidate <- readCandidate
    let passed = viable candidate
    let statement =
            if passed
                then "passed"
                else "failed"
    return statement

candidate1 :: Candidate
candidate1 =
    Candidate
        { candidateId = 1
        , codeReview = A
        , cultureFit = A
        , education = BA
        }

candidate2 :: Candidate
candidate2 =
    Candidate
        { candidateId = 2
        , codeReview = C
        , cultureFit = A
        , education = PhD
        }

candidate3 :: Candidate
candidate3 =
    Candidate
        { candidateId = 3
        , codeReview = A
        , cultureFit = B
        , education = MS
        }

candidateDB :: Map Int Candidate
candidateDB =
    Map.fromList
        [ (1, candidate1)
        , (2, candidate2)
        , (3, candidate3)
        ]

-- Notice that the code structure is very similar to `assessCandidateIO`
-- >>> assessCandidateMaybe 1
-- Just "failed"
-- >>> assessCandidateMaybe 3
-- Just "passed"
-- >>> assessCandidateMaybe 99
-- Nothing
assessCandidateMaybe :: Int -> Maybe String
assessCandidateMaybe id' = do
    candidate <- Map.lookup id' candidateDB
    let passed = viable candidate
    let statement =
            if passed
                then "passed"
                else "failed"
    return statement

{-

Quick check 31.4 Write a simple function Maybe String -> String that will print
failed/passed if there'’'s a result and error id not found for the Nothing constructor

 -}

failPassOrElse :: Maybe String -> String
failPassOrElse Nothing = "Error, id not found"
failPassOrElse (Just x) = x

candidates :: [Candidate]
candidates =
    [ candidate1
    , candidate2
    , candidate3
    ]

-- Again, most of the code is identical to `assessCandidateIO` (and `assessCandidateMaybe`)
assessCandidateList :: [Candidate] -> [String]
assessCandidateList candidates' = do
    candidate <- candidates'
    let passed = viable candidate
    let statement =
            if passed
                then "passed"
                else "failed"
    return statement

assessCandidateList' :: [Candidate] -> [String]
assessCandidateList' candidates' =
    map
        ( \x ->
            if x
                then "passed"
                else "failed"
        )
        passed
  where
    passed = map viable candidates'

{-

This monadic code behaves like map (for lists)

>>> assessCandidateList candidates
["failed","failed","passed"]

This code is identical, however it's less general
>>> assessCandidateList' candidates
["failed","failed","passed"]

 -}

{-

Let's extract the general bit

We can use it with any container

ghci> assessCandidate readCandidate

>>> assessCandidate (Map.lookup 1 candidateDB)
Just "failed"

>>> assessCandidate (Map.lookup 3 candidateDB)
Just "passed"

>>> assessCandidate (Map.lookup 99 candidateDB)
Nothing

>>> assessCandidate candidates
["failed","failed","passed"]

 -}

assessCandidate :: (Monad m) => m Candidate -> m String
assessCandidate mCandidates = do
    candidate <- mCandidates
    let passed = viable candidate
    let statement =
            if passed
                then "passed"
                else "failed"
    return statement
