module Part3 where

import Prelude hiding (map)

-- Recursive types & functions

data ListInt
    = Empty
    | MkList Int ListInt

empty :: ListInt
empty = Empty

oneElement :: ListInt
oneElement = MkList 1 Empty

twoElements :: ListInt
twoElements = MkList 1 (MkList 2 Empty)

count :: ListInt -> Int
count Empty = 0
count (MkList _ rest) = 1 + count rest

sum :: ListInt -> Int
sum Empty = 0
sum (MkList x rest) = x + sum rest

product :: ListInt -> Int
product Empty = 1
product (MkList x rest) = x * product rest

-- produt Empty = 1
-- product [1, 2, 3] = 1 * 2 * 3 * 1
-- product MkList 1 (MkList 2 (MkList 3 Empty))

find :: Int -> ListInt -> Boolean
find _ Empty = false
find x (MkList y rest) =
    if x == y
        then true
        else find x rest

findCount :: Int -> ListInt -> Int
findCount _ Empty = 0
findCount x (MkList y rest) =
    if x == y
        then 1 + findCount x rest
        else findCount x rest


-- prepend 0 [1, 2] => [0, 1, 2]

prepend :: Int -> ListInt -> ListInt
prepend x rest = MkList x rest

-- append 0 [1, 2] => [1, 2, 0]

append :: Int -> ListInt -> ListInt
append x Empty = MkList x Empty
append x (MkList y rest) = MkList y (append x rest)

{- 
append 0 [1, 2]
append 0 (MkList 1 (MkList 2 Empty))
MkList 1 (append 0 (MkList 2 Empty))
MkList 1 (MkList 2 (append 0 Empty))
MkList 1 (MkList 2 (MkList 0 Empty))
-}

-- map (+ 1) [1, 2] => [2, 3]

map :: (Int -> Int) -> ListInt -> ListInt
map _ Empty = Empty
map f (MkList x rest) = MkList (f x) (map f rest)

{-

map (+ 1) [1, 2]
map (+ 1) (MkList 1 (MkList 2 Empty))
MkList (1 + 1) (map f (MkList 2 Empty))
MkList 2 (MkList (2 + 1) (map f Empty))
MkList 2 (MkList 3 Empty)

-}

{-

fold (+) 0 [] = 0
fold (+) 0 [2, 3, 4] = 2 + 3 + 4 + 0

fold (*) 1 [] = 1
fold (*) 1 [2, 3, 4] = 2 * 3 * 4 * 1

-}
fold :: (Int -> Int -> Int) -> Int -> ListInt -> Int
fold f x Empty = x
fold f x (MkList y rest) = f y (fold f x rest)

sum' :: ListInt -> Int
sum' l = fold (+) 0 l

product' :: ListInt -> Int
product' l = fold (*) 1 l

count' :: ListInt -> Int
count' l = fold f' x' l
  where
    x' :: Int
    x' = 0

    f' :: Int -> Int -> Int
    f' _ x = x + 1

findCount' :: Int -> ListInt -> Int
findCount' n l = fold f' x' l
  where
    x' :: Int
    x' = 0

    f' :: Int -> Int -> Int
    f' y x = 
        if y == n 
            then x + 1
            else x


{-
fold (+) 0 [2, 3, 4]
fold (+) 0 (MkList 2 (MkList 3 (MkList 4 Empty)))
2 + (fold (+) 0 (MkList 3 (MkList 4 Empty)))
2 + (3 + (fold (+) 0 (MKList 4 Empty)))
2 + (3 + (4 + (fold (+) 0 Empty)))
2 + (3 + (4 + 0))
2 + 3 + 4 + 0


[2, 3, 4]
MkList 2 (MkList 3 (MKList 4 Empty))
(:) = MkList

2 : 3 : 4 : Empty

fold replaces (:) with (f) and (Empty) with (x)

2 `f` 3 `f` 4 `f` x

(+1) (+1) (+1) 0
1 + 1 + 1 + 0

-}

{- Using the REPL:
In a console in the directory of the project, run: `spago repl`
Then you can do "import ModuleName"
Then you can see types with ":t name"
-}

{-
type arguments -> HKT
other recursive types
-}

-- data ListInt
--     = Empty
--     | MkList Int ListInt

data TreeInt
    = EmptyTree
    | Node TreeInt Int TreeInt

countTree :: TreeInt -> Int
countTree EmptyTree = 0
countTree (Node left x right) = countTree left + 1 + countTree right

sumTree :: TreeInt -> Int
sumTree EmptyTree = 0
sumTree (Node left x right) = sumTree left + x + sumTree right

findTree :: Int -> TreeInt -> Boolean
findTree _ EmptyTree = false
findTree l (Node left x right) =
        if l == x 
            then true
            else findTree l left || findTree l right

{-

1 - 2 - 3 - 4 - 5

      5
     / \
    /   \
   4     10
  / \    /\
 3   E  E  E
 / \
E   2
-}



{------------------- Homework
-}

productTree :: TreeInt -> Int
productTree _ = 0

-- Count how many times we can find 'n' inside 't'
findCountTree :: Int -> TreeInt -> Int
findCountTree n t = 0

-- Apply function 'f' to each value in the tree and return the resulting tree.
mapTree :: (Int -> Int) -> TreeInt -> TreeInt
mapTree f t = t

{- Given some function 'f' which takes 3 ints and returns int
And some default int 'x'
And a tree, then:

if the tree is empty, return 'x'
else we have a Node left n right, so we will do: f "left" n "right"
    where "left" and "right" is the result of folding further left/right

-}
foldTree :: (Int -> Int -> Int -> Int) -> Int -> TreeInt -> Int
foldTree f x t = 0

-- Given this type (identical with ListInt, except we hold strings)
data ListString
    = EmptyLS
    | MkListString String ListString

-- hint: use the function "show :: Int -> String"
-- for example, given
-- MkList        1  (MkList        2  Empty  ), the return should be:
-- MkListString "1" (MkListString "2" EmptyLS)
toString :: ListInt -> ListString
toString l = EmptyLS

-- Given a list string such as ["hello", "world", "42"]
-- the result needs to be "helloworld42"
-- hint: use (<>) :: String -> String -> String
-- e.g.: "hello" <> "world" == "helloworld"
concatenate :: ListString -> String
concatenate _ = ""

-- write mapListString and foldListString