{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use maybe" #-}
import Control.Arrow ((&&&), (>>>))
import Control.Monad (guard)
import Data.Function ((&))
import Data.List (uncons)
import Data.Maybe (fromMaybe)

data Name = Name
    { firstName :: String
    , lastName :: String
    }

instance Show Name where
    show :: Name -> String
    show (Name{firstName, lastName}) =
        mconcat
            [ firstName
            , " "
            , lastName
            ]

data GradeLevel
    = Freshman
    | Sophomore
    | Junior
    | Senior
    deriving (Eq, Ord, Enum, Show)

data Student = Student
    { studentId :: Int
    , gradeLevel :: GradeLevel
    , studentName :: Name
    }
    deriving (Show)

students :: [Student]
students =
    [ Student 1 Senior Name{firstName = "Haskell", lastName = "Curry"}
    , Student 2 Senior Name{firstName = "Alan", lastName = "Turing"}
    , Student 3 Freshman Name{firstName = "Dennis", lastName = "Ritchie"}
    , Student 4 Junior Name{firstName = "Yukihiro", lastName = "Matsumoto"}
    , Student 5 Sophomore Name{firstName = "Guido", lastName = "van Rossum"}
    , Student 6 Sophomore Name{firstName = "John", lastName = "Backus"}
    , Student 7 Sophomore Name{firstName = "Ada", lastName = "Lovelace"}
    , Student 8 Junior Name{firstName = "Alan", lastName = "Kay"}
    ]

{-

Show the (whole) student names
>>> select_ (studentName) students
[Haskell Curry,Alan Turing,Dennis Ritchie,Yukihiro Matsumoto,Guido van Rossum]

Show the student first names
>>> select_ (firstName . studentName) students
["Haskell","Alan","Dennis","Yukihiro","Guido"]

>>> select_ gradeLevel students
[Senior,Senior,Freshman,Junior,Sophomore]
 -}
select_ :: (a -> b) -> [a] -> [b]
select_ prop vals = do
    val <- vals
    return (prop val)

selectFirstNames :: [String]
selectFirstNames = select_ (firstName . studentName) students

selectFirstNames2 :: [String]
selectFirstNames2 = select_ (studentName >>> firstName) students

{-

>>> selectMultiple
[("Haskell",Senior),("Alan",Senior),("Dennis",Freshman),("Yukihiro",Junior),("Guido",Sophomore)]

>>> selectMultiple2
[("Haskell",Senior),("Alan",Senior),("Dennis",Freshman),("Yukihiro",Junior),("Guido",Sophomore)]
 -}
selectMultiple :: [(String, GradeLevel)]
selectMultiple =
    select_
        ( \x ->
            ( (firstName . studentName) x
            , gradeLevel x
            )
        )
        students

selectMultiple2 :: [(String, GradeLevel)]
selectMultiple2 =
    select_
        ( \x ->
            ( x & studentName & firstName
            , x & gradeLevel
            )
        )
        students

{-

Worth noting that `select_` is basically `fmap`

>>> selectMultiple3
[("Haskell",Senior),("Alan",Senior),("Dennis",Freshman),("Yukihiro",Junior),("Guido",Sophomore)]

>>> selectMultiple4
[("Haskell",Senior),("Alan",Senior),("Dennis",Freshman),("Yukihiro",Junior),("Guido",Sophomore)]

 -}
selectMultiple3 :: [(String, GradeLevel)]
selectMultiple3 =
    fmap
        ( \x ->
            ( (firstName . studentName) x
            , gradeLevel x
            )
        )
        students

selectMultiple4 :: [(String, GradeLevel)]
selectMultiple4 =
    ( \x ->
        ( (firstName . studentName) x
        , gradeLevel x
        )
    )
        <$> students

where_ :: (a -> Bool) -> [a] -> [a]
where_ test vals = do
    val <- vals
    guard $ test val
    return val

{-

>>> seniors
[Haskell Curry,Alan Turing]
 -}
-- seniors :: [Student]
seniors :: [Name]
seniors =
    students
        -- & where_ (\student -> Senior == gradeLevel student)
        & where_ ((== Senior) . gradeLevel)
        & select_ studentName

{-
>>> startsWith 'a' "abc"
True

>>> startsWith 'a' ""
False

 -}

startsWith :: Char -> String -> Bool
startsWith _ "" = False
startsWith c (x : _xs) = c == x

startsWith2 :: Char -> String -> Bool
startsWith2 c str =
    maybe
        False
        (\(x, _xs) -> x == c)
        (uncons str)

startsWith3 :: Char -> String -> Bool
startsWith3 c str =
    fromMaybe False $
        (\(x, _xs) -> x == c)
            <$> uncons str

{-

>>> students1
[Alan Turing,Ada Lovelace,Alan Kay]

>>> students2
[(2,"Alan"),(7,"Ada"),(8,"Alan")]

>>> students3
[(2,("Alan",("Turing",True))),(7,("Ada",("Lovelace",False))),(8,("Alan",("Kay",False)))]

 -}

students1 :: [Name]
students1 =
    where_
        (startsWith 'A' . firstName)
        (select_ studentName students)

students2 :: [(Int, String)]
students2 =
    students
        & where_ (startsWith 'A' . firstName . studentName)
        & select_ (\x -> (studentId x, (firstName . studentName) x))

-- I can use the tuple constructor (&&&) from Control.Arrow
-- But it creates nested tuples
students3 :: [(Int, (String, (String, Bool)))]
students3 =
    students
        & where_ (startsWith 'A' . firstName . studentName)
        & select_
            ( studentId
                &&& (firstName . studentName)
                &&& (lastName . studentName)
                &&& ((== Senior) . gradeLevel)
            )

data Teacher = Teacher
    { teacherId :: Int
    , teacherName :: Name
    }
    deriving (Show)

teachers :: [Teacher]
teachers =
    [ Teacher 100 (Name "Simone" "De Beauvior")
    , Teacher 200 (Name "Susan" "Sontag")
    ]

data Course = Course
    { courseId :: Int
    , courseTitle :: String
    , teacher :: Int
    }
    deriving (Show)

courses :: [Course]
courses =
    [ Course 101 "French" 100
    , Course 201 "English" 200
    ]

{-

ghci> mapM_ print $ join_ teachers courses teacherId teacher
(Teacher {teacherId = 100, teacherName = Simone De Beauvior},Course {courseId = 101, courseTitle = "French", teacher = 100})
(Teacher {teacherId = 200, teacherName = Susan Sontag},Course {courseId = 201, courseTitle = "English", teacher = 200})

 -}
join_ :: (Eq c) => [a] -> [b] -> (a -> c) -> (b -> c) -> [(a, b)]
join_ as bs propA propB = do
    a <- as
    b <- bs
    let pairs = (a, b)
    guard $ propA a == propB b
    return pairs

{-

33.4 Building your HINQ interface

 -}

joinData :: [(Teacher, Course)]
joinData = join_ teachers courses teacherId teacher

whereResult :: [(Teacher, Course)]
whereResult = where_ ((== "English") . courseTitle . snd) joinData

selectResult :: [Name]
selectResult = select_ (teacherName . fst) whereResult

{-

Well, we already have an API, programmatically

>>> myQuery
[Susan Sontag]

>>> myQuery2
[Susan Sontag]
 -}
myQuery :: [Name]
myQuery =
    join_ teachers courses teacherId teacher
        & where_ ((== "English") . courseTitle . snd)
        & select_ (teacherName . fst)

-- Do it normal application style
{- FOURMOLU_DISABLE -}
myQuery2 :: [Name]
myQuery2 =
    select_ (teacherName . fst)
    $ where_ ((== "English") . courseTitle . snd)
    $ join_ teachers courses teacherId teacher
{- FOURMOLU_ENABLE -}

hinq_ :: (a -> b) -> c -> (c -> a) -> b
hinq_ selectQuery joinQuery whereQuery =
    ( \joinData ->
        ( \whereData ->
            selectQuery whereData
        )
            (whereQuery joinData)
    )
        joinQuery

{-

We can reorder our query fragments, like so:
>>> myQuery3
[Susan Sontag]

 -}
myQuery3 :: [Name]
myQuery3 =
    hinq_
        (select_ (teacherName . fst))
        (join_ teachers courses teacherId teacher)
        (where_ ((== "English") . courseTitle . snd))

{-

>>> myQuery4
[Simone De Beauvior,Susan Sontag]

 -}
myQuery4 :: [Name] -- without a where clause
myQuery4 =
    hinq_
        (select_ (teacherName . fst))
        (join_ teachers courses teacherId teacher)
        (where_ (const True))
