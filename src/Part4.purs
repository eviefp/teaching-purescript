module Part4 where

import Prelude

{-
We saw how we can create new types using the `data` keyword.
-}

-- data Point = Point Int Int Int

-- But then, this isn't really very easy to use. Which one is 'y'?

-- `type`s are really just aliases for convenience
type Point =
    { x :: Int
    , y :: Int
    , z :: Int
    }

-- Creating a new 'Point':
origin :: Point
origin = { x: 0, y: 0, z: 0 }

-- The `Point` type is just an alias, we could just use the expanded version as well:
origin' :: { x :: Int, y :: Int, z :: Int }
origin' = origin

-- Get 'x':
getX :: Point -> Int
getX p = p.x

-- Increment all coordinates by 1
increment :: Point -> Point
increment p = { x: p.x + 1, y: p.y + 1, z: p.z + 1 }

increment' :: Point -> Point
increment' { x, y, z} = { x: x + 1, y: y + 1, z: z + 1 }

increment'' :: Point -> Point
increment'' p = p + {x: 1, y: 1, z: 1}

------------------------------------------

data TreeInt
    = EmptyTree
    | Node
        { left :: TreeInt
        , value :: Int
        , right :: TreeInt
        }

mapTree :: (Int -> Int) -> TreeInt -> TreeInt
mapTree _ EmptyTree = EmptyTree
mapTree f (Node n) = 
    Node
        { left: mapTree f n.left
        , value: f n.value
        , right: mapTree f n.right
        }

------------------------------------------

type Point2D = { x :: Int, y :: Int }
type Point3D = { x :: Int, y :: Int, z :: Int }

{- Type variables:

- allow us to abstract over types

Syntax is:

functionName :: forall r. <we can use 'r' here>

-}

getX' :: forall rest. { x :: Int | rest } -> Int
getX' p = p.x

-- examples of usages:

-- In this case, 'rest' is NOTHING
get1 :: Int
get1 = getX' { x: 0 }

-- In this case, 'rest' is 'y :: Int'
get2 :: Int
get2 = getX' { x: 0, y: 1}

-- In this case, (y :: Int, z :: Int)
get3 :: Int
get3 = getX' { x: 0, y: 1, z: 2}

-- In this case, (bar :: String, blah :: String)
get4 :: Int
get4 = getX' { bar: "baz", x: 0, blah: "foo" }

-------------------------------------------------------------

type Person = { name :: String, age :: Int }

getName :: Person -> String
getName p1 = p1.name

getAge :: Person -> Int
getAge p1 = p1.age

{-
I want a function, `print`, which I want to use as:

print { name: "Vlad", age: 36 }
> "Vlad is 36 years old"
-}

print' :: Person -> String
print' p1 = p1.name <> " is " <>  show p1.age <> " years old"

-------------------------------------
-- Typeclasses
-------------------------------------

{-
Until now, we learned about:
- Types (`type`, `data`, etc.)
- Functions, Int -> Int

-}

class Print a where
    print :: a -> String

instance printInt :: Print Int where
    print :: Int -> String
    print i = show i

instance printString :: Print String where
    print :: String -> String
    print s = show s

-- IMPORTANT: We can't declare instances for 'type's

-- This will fail, because we have no instance for Boolean:
-- test :: String
-- test = print true

data MyType = MyType Int

instance printType :: Print MyType where
  print :: MyType -> String
  print (MyType x) = show x

data MyType2 = MyType2 Int String

instance printType2 :: Print MyType2 where
  print :: MyType2 -> String
  print (MyType2 int string) = print int <> print string

-------------

-- In Purescript, this is called class Eq
class Equal a where
   equal :: a -> a -> Boolean -- This is called 'eq' in Purescript

-- There exists an alias (==) which is the same as 'eq'

instance equalInt :: Equal Int where
    equal :: Int -> Int -> Boolean
    equal x y = x == y

instance equalString :: Equal String where
    equal :: String -> String -> Boolean
    equal x y = x == y

instance equalMyType2 :: Equal MyType2 where
  equal :: MyType2 -> MyType2 -> Boolean
  equal (MyType2 int1 string1) (MyType2 int2 string2) = int1 == int2 && string1 == string2 

{- How would you define notEqual?
-}
defaultNotEqual :: forall a. Equal a => a -> a -> Boolean
defaultNotEqual x y = not (equal x y)

-- So now, we can say:
test' :: Boolean
test' = defaultNotEqual 1 2

-----------------------------------------------
-- Comparing two values

-- data Ordering = LT | GT | EQ
data CompareResult = Equals | Greater | Lower

-- class Eq a <= Ord a
class Equal a <= Compare a where
    compare :: a -> a -> CompareResult

instance compareInt :: Compare Int where
    compare x y
      | x > y     = Greater
      | x < y     = Lower
      | otherwise = Equals

instance compareString :: Compare String where
  compare x y 
    | x > y   = Greater 
    | x < y   = Lower
    | otherwise = Equals

--------------------------------
-- Homework
--------------------------------

-- We cannot define instances for this!
type Point' = { x :: Int, y :: Int }

somePoint :: Point'
somePoint = { x: 1, y: 2 }

getX'' :: Point' -> Int
getX'' p = p.x

use :: Int
use = getX'' somePoint


-- When doing this, we can define instances for this type!
data DPoint = MkDPoint { x :: Int, y :: Int}

someDPoint :: DPoint
someDPoint = MkDPoint { x: 1, y: 2}

getDX :: DPoint -> Int
getDX (MkDPoint p) = p.x

useD :: Int
useD = getDX someDPoint


-- 1. Write a 'Print' instance for 'DPoint'
-- 2. Write a 'Equal' instance for 'DPoint'
-- 3. Write an 'Compare' instance for 'DPoint' where the 'x' is "more important"

-- 4. Take the ListInt type from lesson 3. Declare the 3 instances above for it as well.
-- The same idea applies: [3, 2, 5] > [2, 10, 10] (first different element decides comparison)
-- [3, 2, 5] < [3, 2, 6]
-- [3, 2] < [3, 2, 0]

data ListInt
    = Empty
    | MkList Int ListInt

-- 5. Do the same for TreeInt

-- 6. Think about and define a class that can concatenate two ListInt or two
-- ListString

concatenateListInt :: ListInt -> ListInt -> ListInt
concatenateListInt Empty l2 = l2
concatenateListInt (MkList int1 listint1) list2 = 
           MkList int1 (concatenateListInt listint1 list2)

-- class Concatenating a where
--     concat :: ???

-- instance concatenatingListInt :: Concatenating ListInt where
--    ...

-- instance concatenatingListString :: Concatenating ListString where
--    ...

-- concat (MkList 1 Empty) (MkList 2 Empty)
-- concat (MkStringList "1" EmptyLS) (MkStringList "2" EmptyLS)

-- 7. Could you define an instance of 'Concatenating' for String where we (<>) strings together?
-- 8. Could you define an instance of 'Concatenating' for Int where we add numbers?
-- 9. The same question, but for booleans using the operation (&&)?