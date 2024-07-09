{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use id" #-}
module Lesson10 where

{- |

flOz stands for fluid ounces.

>>> coffee = cup 12


>>> getOz coffee
12

>>> afterASip = drink coffee 1

>>> getOz afterASip
11

>>> finish = drink coffee 99

>>> getOz finish
0

>>> isEmpty coffee
False

>>> isEmpty finish
True

To avoid having to come up with intermediate identifiers, we can use foldl to represent state change

>>> afterManySips = foldl drink coffee [1,1,2]

>>> getOz afterManySips
8
-}
cup flOz = \message -> flOz

getOz aCup = aCup (\flOz -> flOz)

drink aCup ozDrank = cup $ max 0 (flOz - ozDrank)
  where
    flOz = getOz aCup

isEmpty aCup = getOz aCup == 0

--- ROBOT

type Robot = (String, Int, Int)
type RobotInstance msg = (Robot -> msg) -> msg

-- An object can be viewed as a collection of attributes that you send messages to.
newRobot :: Robot -> RobotInstance a
newRobot (name, attack, hp) = \message -> message (name, attack, hp)

{- |

>>> killerRobot id
("Kill3r",25,200)

>>> name $ killerRobot id
"Kill3r"

>>> getName killerRobot
"Kill3r"

>>> getAttack killerRobot
25

>>> getHP killerRobot
200
-}
killerRobot :: RobotInstance msg
killerRobot = newRobot ("Kill3r", 25, 200)

name :: Robot -> String
name (str, _, _) = str

attack :: Robot -> Int
attack (_, n, _) = n

hp :: Robot -> Int
hp (_, _, n) = n

{- |

>>> getName killerRobot
"Kill3r"

>>> getName' killerRobot
"Kill3r"

>>> getName'' killerRobot
"Kill3r"

So, a getter is a function that takes another function accepting a Robot as input, and returns a value.
We could also say that this function represents the "robot's instance"
-}

-- GETTERS

getName :: RobotInstance String -> String
getName = \aRobot -> aRobot name

getName' :: ((Robot -> String) -> t) -> t
getName' = \aRobot -> aRobot (\(name, _, _) -> name)

getName'' :: ((Robot -> String) -> t) -> t
getName'' aRobot = aRobot (\(name, _, _) -> name)

getAttack :: RobotInstance Int -> Int
getAttack aRobot = aRobot attack

getHP :: ((Robot -> Int) -> t) -> t
getHP aRobot = aRobot hp

-- SETTERS

{- |

>>> getName killerRobot
"Kill3r"

>>> r2d2 = setName killerRobot "R2-D2"

>>> getName r2d2
"R2-D2"
-}
setName :: ((Robot -> RobotInstance a) -> RobotInstance a) -> String -> RobotInstance a
setName aRobot newName = aRobot $ \(_, attack, hp) -> newRobot (newName, attack, hp)

setAttack :: ((Robot -> RobotInstance a) -> RobotInstance a) -> Int -> RobotInstance a
setAttack aRobot newAttack = aRobot $ \(name, _, hp) -> newRobot (name, newAttack, hp)

setHP :: ((Robot -> RobotInstance a) -> RobotInstance a) -> Int -> RobotInstance a
setHP aRobot newHP = aRobot $ \(name, attack, _) -> newRobot (name, attack, newHP)

kitty :: RobotInstance a
kitty = setName killerRobot "Kitty"

gentlerRobot :: RobotInstance a
gentlerRobot = setAttack killerRobot 5

softerRobot :: RobotInstance a
softerRobot = setHP killerRobot 50

robotToString :: RobotInstance String -> String
robotToString aRobot = aRobot (\(name, attack, hp) -> name ++ " attack:" ++ show attack ++ " hp:" ++ show hp)

{- |

>>> robotToString killerRobot
"Kill3r attack:25 hp:200"

>>> robotToString kitty
"Kitty attack:25 hp:200"

>>> robotToString gentlerRobot
"Kill3r attack:5 hp:200"

>>> robotToString softerRobot
"Kill3r attack:25 hp:50"
-}

{- |
>>> getHP killerRobot
200
>>> afterHit = damage killerRobot 10
>>> getHP afterHit
190
-}
damage :: ((Robot -> RobotInstance a) -> RobotInstance a) -> Int -> RobotInstance a
damage aRobot damage = aRobot $ \(name, attack, hp) -> newRobot (name, attack, hp - damage)

{- |

>>> robotToString krRound3
"Kill3r attack:25 hp:155"
>>> robotToString srRound3
"Softy attack:10 hp:225"
-}
fight :: RobotInstance Int -> ((Robot -> RobotInstance a) -> RobotInstance a) -> RobotInstance a
fight aRobot defender = damage defender force
  where
    force =
        if getHP aRobot > 10
            then getAttack aRobot
            else 0

-- NOTE: Unlike other languages, Haskell does not care: we can reorder these function calls and get
--       the same result!
--       This behaviour may be advantageous, think concurrent programming!
srRound1 = fight killerRobot (newRobot ("Softy", 10, 300))
krRound1 = fight softerRobot killerRobot

srRound2 = fight krRound1 srRound1
krRound2 = fight srRound1 krRound1

srRound3 = fight krRound2 srRound2
krRound3 = fight srRound2 krRound2

{- | ex1

>>> getLives
[200,200,50]
-}
getLives :: [Int]
getLives = map getHP [killerRobot, gentlerRobot, softerRobot]

{- | ex2
rounds :: RobotInstance Int -> RobotInstance Int -> String
-}

-- robotA :: RobotInstance a
-- robotA = newRobot ("Robot A", 10, 100)

-- robotB :: RobotInstance a
-- robotB = newRobot ("Robot B", 25, 80)

{- |

>>> robotToString rounds
"Robot B attack:25 hp:40"
-}

-- rounds :: RobotInstance a
rounds =
    -- fight robotB (fight robotB (fight robotB robotA))
    let
        a =
            compete
                ( newRobot ("Robot A", 10, 100)
                , newRobot ("Robot B", 25, 80)
                )

        b =
            compete
                ( newRobot ("Robot B", 25, 80)
                , newRobot ("Robot A", 10, 100)
                )
     in
        if getHP a > getHP b then a else b
  where
    compete (a, b) =
        fight
            b
            ( fight
                b
                (fight b (fight b a))
            )

{- | ex3
>>> map robotToString ex3
["Robot A attack:10 hp:85","Robot B attack:25 hp:65","Robot C attack:15 hp:105"]
-}
ex3 :: [RobotInstance a]
ex3 =
    let
        robots =
            [ newRobot ("Robot A", 10, 100)
            , newRobot ("Robot B", 25, 80)
            , newRobot ("Robot C", 15, 120)
            ]
     in
        map (fight (newRobot ("Fighter", 15, 120))) robots
