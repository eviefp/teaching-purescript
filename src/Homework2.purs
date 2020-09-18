module Homework2 where

import Prelude
 
-- 1. Define a type to hold information about a user: a username (as string),
-- an email (also string), and age (as int).
data User = MkUser String String Int

{-
User is the new type created
-}

utilizator1 :: User
utilizator1 = MkUser "AAA" "AMail" 24

utilizator2 :: User
utilizator2 = MkUser "BBB" "BMail" 25

utilizator3 :: User
utilizator3 = MkUser "CCC" "CMail" 40

utilizator4 :: User
utilizator4 = MkUser "DDD" "DMail" 35 

{-
 Added 4 users
-}

getUser :: User -> String
getUser (MkUser x _ _) = x

getMail :: User -> String
getMail (MkUser _ y _) = y

getAge :: User -> Int
getAge (MkUser _ _ z) = z

ageIncrement :: User -> User
ageIncrement (MkUser username email age) = MkUser username email (age + 1)

oldestUser :: User -> User -> User
oldestUser user1@(MkUser _ _ a1) user2@(MkUser _ _ a2) =
     if a1 > a2
         then user1
         else user2

-- 5. Write a new type `Person` that has the following constructors:
--     - `Employee` with a `Boolean` property which represents whether he is a new employee
--     - `Manager` with two `Boolean` properties:
--               whether he is also an admin, and whether they have their own office

data Person
    = Employee Boolean
    | Manager Boolean Boolean


isEmployee :: Person -> Boolean
isEmployee (Employee _) = true
isEmployee (Manager _ _) = false

-- is manager AND is admin AND has own office

truex3 :: Person -> Boolean
truex3 (Employee _) = false
truex3 (Manager admin office) = admin && office

{-

admin | admin == true 
true  | true
false | false

admin | office | admin && office
true  | true   | true
---            | false

if boolean then true else false
boolean

boolean could be true, false
boolean could be a || b && c

-}

someManager :: Person
someManager = Manager false true

-- 8. Write a function `Person -> Person` which makes the `Manager` an admin regardless of
-- what they were before, and leaves the `Employee` as before.

sysPromotion :: Person -> Person
sysPromotion (Employee x) = Employee x
sysPromotion (Manager _ office) = Manager true office



-- sysPromotion (MkManager x y z) = _ true  z
-- sysPromotion x y z = x y z
 