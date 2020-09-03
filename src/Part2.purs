module Part2 where

import Prelude

-- Pattern matching

-- if condition then first, else second
cond :: Boolean -> String -> String -> String
cond true  s1 s2 = s1
cond false s1 s2 = s2

-- if first is "hello", then return "hello"
-- else if condition then first, else second
cond' :: Boolean -> String -> String -> String
cond' b     "hello" s2 = "hello"
cond' true  s1      s2 = s1
cond' false s1      s2 = s2

data MyBools = ConstructMyBools Boolean

myTrue :: MyBools
myTrue = ConstructMyBools true

myFalse :: MyBools
myFalse = ConstructMyBools false

booleanToMyBools :: Boolean -> MyBools
booleanToMyBools b = ConstructMyBools b

myBoolsToBoolean :: MyBools -> Boolean
myBoolsToBoolean (ConstructMyBools b) = b

myCond :: MyBools -> String -> String -> String
myCond (ConstructMyBools true)  s1 s2 = s1
myCond (ConstructMyBools false) s1 s2 = s2


data Point = MkPoint Int Int

origin :: Point
origin = MkPoint 0 0

somePoint :: Point
somePoint = MkPoint 1 2
-- X: 1 | Y: 2

getX :: Point -> Int
getX (MkPoint x _) = x

getY :: Point -> Int
getY (MkPoint _ y) = y

incrementXBy5 :: Point -> Point
incrementXBy5 (MkPoint x y) = MkPoint (x + 5) y


add_3_numbers_easy :: Int -> Int -> Int -> Int
add_3_numbers_easy x y z = x + y + z


data Weekday
    = Mon | Tue | Wed | Thu | Fri | Sat | Sun

toInt :: Weekday -> Int
toInt Mon = 1
toInt Tue = 2
toInt Wed = 3
toInt Thu = 4
toInt Fri = 5
toInt Sat = 6
toInt Sun = 7

toInt' :: Weekday -> Int
toInt' weekday = case weekday of
   Mon -> 1
   Tue -> 2
   Wed -> 3
   Thu -> 4
   Fri -> 5
   Sat -> 6
   Sun -> 7