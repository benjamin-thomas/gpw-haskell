{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use bimap" #-}
import Control.Applicative (Alternative)
import Control.Monad (guard)
import Data.List (uncons)

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

data Enrollment = Enrollment
    { student :: Int
    , course :: Int
    }
    deriving (Show)

enrollments :: [Enrollment]
enrollments =
    [ Enrollment 1 101
    , Enrollment 2 101
    , Enrollment 2 201
    , Enrollment 3 201
    , Enrollment 4 101
    , Enrollment 4 201
    , Enrollment 5 101
    , Enrollment 6 201
    ]

{-

33.5 Making a HINQ type

 -}

select_ :: (Monad m) => (a -> b) -> m a -> m b
select_ prop vals = do
    prop <$> vals

where_ :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
where_ test vals = do
    val <- vals
    guard $ test val
    return val

join_ :: (Monad m, Alternative m, Eq c) => m a -> m b -> (a -> c) -> (b -> c) -> m (a, b)
join_ as bs propA propB = do
    a <- as
    b <- bs
    let pairs = (a, b)
    guard $ propA a == propB b
    return pairs

data HINQ m a b
    = HINQ (m a -> m b) (m a) (m a -> m a)
    | HINQ_ (m a -> m b) (m a)

hinq_ :: (a -> b) -> c -> (c -> a) -> b
hinq_ selectQuery joinQuery whereQuery =
    (selectQuery . whereQuery) joinQuery

runHINQ :: (Monad m, Alternative m) => HINQ m a b -> m b
runHINQ (HINQ sClause jClause wClause) = hinq_ sClause jClause wClause
runHINQ (HINQ_ sClause jClause) = hinq_ sClause jClause (where_ $ const True)

{-
>>> runHINQ query1
[Susan Sontag]
 -}
query1 :: HINQ [] (Teacher, Course) Name
query1 =
    HINQ
        (select_ (teacherName . fst))
        (join_ teachers courses teacherId teacher)
        (where_ ((== "English") . courseTitle . snd))

{-
>>> runHINQ query2r
[Simone De Beauvior,Susan Sontag]
 -}
query2 :: HINQ [] Teacher Name
query2 = HINQ_ (select_ teacherName) teachers

possibleTeacher :: Maybe Teacher
possibleTeacher =
    fst <$> uncons teachers

possibleCourse :: Maybe Course
possibleCourse =
    fst <$> uncons courses

{-

>>> runHINQ maybeQuery1
Just Simone De Beauvior

 -}
maybeQuery1 :: HINQ Maybe (Teacher, Course) Name
maybeQuery1 =
    HINQ
        (select_ (teacherName . fst))
        (join_ possibleTeacher possibleCourse teacherId teacher)
        (where_ ((== "French") . courseTitle . snd))

missingCourse :: Maybe Course
missingCourse = Nothing

{-

>>> runHINQ maybeQuery2
Nothing

 -}
maybeQuery2 :: HINQ Maybe (Teacher, Course) Name
maybeQuery2 =
    HINQ
        (select_ (teacherName . fst))
        (join_ possibleTeacher missingCourse teacherId teacher)
        (where_ ((== "French") . courseTitle . snd))

{-
33.23 Student enrollments

>>> runHINQ studentEnrollmentsQ
[(Haskell Curry,101),(Alan Turing,101),(Alan Turing,201),(Dennis Ritchie,201),(Yukihiro Matsumoto,101),(Yukihiro Matsumoto,201),(Guido van Rossum,101),(John Backus,201)]
 -}

studentEnrollmentsQ :: HINQ [] (Student, Enrollment) (Name, Int)
studentEnrollmentsQ =
    HINQ_
        ( select_
            ( \(st, en) ->
                (studentName st, course en)
            )
        )
        (join_ students enrollments studentId student)

studentEnrollments :: [(Name, Int)]
studentEnrollments = runHINQ studentEnrollmentsQ

{-

>>> runHINQ englishStudentsQ
[Alan Turing,Dennis Ritchie,Yukihiro Matsumoto,John Backus]
 -}
englishStudentsQ :: HINQ [] ((Name, Int), Course) Name
englishStudentsQ =
    HINQ
        (select_ (fst . fst))
        (join_ studentEnrollments courses snd courseId)
        (where_ ((== "English") . courseTitle . snd))

englishStudents :: [Name]
englishStudents = runHINQ englishStudentsQ

{-

>>> getEnrollments "English"
[Alan Turing,Dennis Ritchie,Yukihiro Matsumoto,John Backus]

 -}
getEnrollments :: String -> [Name]
getEnrollments courseName = runHINQ courseQuery
  where
    courseQuery :: HINQ [] ((Name, Int), Course) Name
    courseQuery =
        HINQ
            (select_ (fst . fst))
            (join_ studentEnrollments courses snd courseId)
            (where_ ((== courseName) . courseTitle . snd))
