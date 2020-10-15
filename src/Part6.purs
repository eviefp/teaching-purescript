module Part6 where

import Prelude


-- Products
data Point = Point Int Int

-- Sums
data WeekendDays = Sat | Sun

-- Recursive types
data ListInt = EmptLI | ConsLI Int ListInt

data ListString = EmptLS | ConsLS String ListString

data ListPoint = EmptLP | ConsLP Point ListPoint

---------------------------------------------------------------

data List a = Nil | Cons a (List a)

-- []
empty :: forall a. List a
empty = Nil

-- [1]
oneList :: List Int
oneList = Cons 1 empty

-- [2, 1]
twoList :: List Int
twoList = Cons 2 oneList

count :: forall a. List a -> Int
count Nil = 0
count (Cons _ rest) = 1 + count rest

-- find :: Int -> ListInt -> Boolean
find :: forall a. Eq a => a -> List a -> Boolean
find _ Nil = false
find a (Cons y rest) =
  if a == y
    then true
    else find a rest

data KeepOrDiscard = Keep | Discard

discardOnes :: Int -> KeepOrDiscard
discardOnes 1 = Discard
discardOnes _ = Keep

-- [1, 2, 3]
-- and we want to filter out the elements that are equal to 1
-- then the result should be [2, 3]
-- filter discardOnes [1, 2, 3] == [2, 3]
filter :: forall a. (a -> KeepOrDiscard) -> List a -> List a
filter _ Nil = Nil
filter pred (Cons a rest) =
  case pred a of
    Keep -> Cons a (filter pred rest)
    Discard -> filter pred rest

-- prepend :: Int -> ListInt -> ListInt
-- prepend x rest = MkList x rest
-- prepend 1 [2, 3] == [1, 2, 3]
-- prepend "hi" ["hello", "world"] == ["hi", "hello", "world"]
prepend :: forall a. a -> List a -> List a
prepend a rest = Cons a rest

-- append :: Int -> ListInt -> ListInt
-- append x Empty = MkList x Empty
-- append x (MkList y rest) = MkList y (append x rest)
-- append 1 [2, 3] == [2, 3, 1]
append :: forall a. a -> List a -> List a
append a Nil = Cons a Nil
append a (Cons b rest) = Cons b (append a rest)

-- Maybe
  
-- data List a = Nil | Cons a (List a)
data Maybe a = Nothing | Just a

none :: forall a. Maybe a
none = Nothing

one :: Maybe Int
one = Just 1

hello :: Maybe String
hello = Just "hello"

-- isNothing none   == true
-- isNothing one    == false
-- isNothing hello  == false

isNothing :: forall a. Maybe a -> Boolean
isNothing Nothing = true
isNothing _ = false

isNothing1 :: forall a. Maybe a -> Boolean
isNothing1 Nothing = true
isNothing1 (Just _) = false
 
isNothing2 :: forall a. Maybe a -> Boolean
isNothing2 m = case m of
  Nothing -> true
  Just _ -> false

isNothing3 :: forall a. Maybe a -> Boolean
isNothing3 = case _ of
  Nothing -> true
  Just _ -> false

-- mapMaybe (_ + 1) Nothing  == Nothing
-- mapMaybe (_ + 1) (Just 1) == Just 2
-- mapMaybe show    (Just 1) == Just "1"
mapMaybe :: forall a b. (a -> b) -> Maybe a -> Maybe b
mapMaybe a2b (Nothing) = Nothing
mapMaybe a2b (Just a) = Just (a2b a)

fromMaybe :: forall a. a -> Maybe a -> a
fromMaybe _   (Just a) = a
fromMaybe def Nothing  = def

-- catamorphism
maybe :: forall a b. b -> (a -> b) -> Maybe a -> b
maybe _ a2b (Just a) = a2b a
maybe b _   Nothing  = b

--------

-- data List a = Nil | Cons a (List a)
-- data Maybe a = Nothing | Just a
data Either a b = Left a | Right b

-- Some example of using Eithers
divide :: Int -> Int -> Either String Int
divide _ 0 = Left "Error, cannot divide by 0."
divide n m = Right (n / m)

isLeft :: forall a b. Either a b -> Boolean
isLeft (Left _) = true
isLeft (Right _) = false

mapLeft :: forall a b c. (a -> c) -> Either a b -> Either c b
mapLeft a2c (Left a) = Left (a2c a)
mapLeft _   (Right b) = Right b

mapRight :: forall a b c. (b -> c) -> Either a b -> Either a c 
mapRight b2c (Right b) = Right (b2c b)
mapRight b2c (Left a) = Left a


------------------------------------------

-- These are all Higher Kinded Types
-- data List a = Nil | Cons a (List a)
-- data Maybe a = Nothing | Just a
-- data Either a b = Left a | Right b

-- 'List' on its own, makes no SENSE
-- 'List a' 'List Int', 'List String', "List (List (List String))"
-- 'Maybe a'
-- 
-- f :: Int -> Int
--
-- Int :: Type
-- String :: Type
-- List :: Type -> Type
-- List Int :: Type
-- Maybe :: Type -> Type
-- Either :: Type -> Type -> Type
-- Either String :: Type -> Type
-- Either String Int :: Type
-- (->) :: Type -> Type -> Type
-- Int -> String :: Type
-- ((->) Int) :: Type -> Type kind of like (Int -> _), but that's not the correct syntax

-- Here, 'f' is totally unknown. It can be anything.
class Functor' f where
   -- Here, when we write 'f a' and 'f b', the compiler now knows that f must be
   -- f :: Type -> Type
   -- We also know that
   -- a :: Type
   -- b :: Type
   map' :: forall a b. (a -> b) -> f a -> f b


instance maybeFunctor :: Functor' Maybe where
  map' :: forall a b. (a -> b) -> Maybe a -> Maybe b
  map' a2b ma = mapMaybe a2b ma


-- map' show [1, 2, 3] == ["1", "2", "3"]
instance listFunctor :: Functor' List where
  map' :: forall a b. (a -> b) -> List a -> List b
  map' _ Nil = Nil
  map' a2b (Cons a rest) = (Cons (a2b a) (map' a2b rest))

instance eitherFunctor :: Functor' (Either r) where 
   map' :: forall a b. (a -> b) -> Either r a -> Either r b
   map' a2b (Left r) = Left r
   map' a2b (Right a) =
      let b = a2b a
      in Right b



-- For any higher kinded type T
-- if it has more than one argument, e.g.
-- T a b c
-- we can only use Functor to "change" the LAST argument
-- So we can go from `T a b c` to `T a b d` using a `c -> d` function


----------------- HOMEWORK

-- returns true if the constructor is 'Just'
isJust :: forall a. Maybe a -> Boolean
isJust _ = true

-- returns true if the constructor is 'Right'
isRight ::  forall a b. Either a b -> Boolean
isRight _ = true

-- note "Missing value" Nothing  == Left "Missing value"
-- note "Missing value" (Just 1) == Right 1
note :: forall a b. a -> Maybe b -> Either a b
note a _ = Left a

-- hush (Left "Missing Value") == Nothing
-- hush (Right 1) == Just 1
hush :: forall a b. Either a b -> Maybe b
hush _ = Nothing

-- Returns how many elements are in total.
length :: forall a. List a -> Int
length _ = 0

-- index 0 [1, 2, 3] == Just 1
-- index 1 [1, 2, 3] == Just 2
-- index 10 [1, 2, 3] == Nothing
index :: forall a. Int -> List a -> Maybe a
index _ _ = Nothing

-- Hint: we also defined it for ListInt. Maybe it helps :-)
fold :: forall a b. b -> (a -> b -> b) -> List a -> b
fold b _ _ = b

-- [Hard] Note that you have to use both <> and mempty to solve this:
fold' :: forall m. Monoid m => List m -> m
fold' _ = mempty


-- Define Functor instances for the following types:

-- Simple
data Identity a = Identity a


-- Product types
data Pair a = Pair a a

data TwoPair a b = TwoPair a b

data ThreePair a b c = ThreePair a b c

data OtherPair a b = OtherPair b a b

-- Sum types
data SumOne a = One | Two | Three a

data SumTwo a b c = This a | That b | Other c

-- Recursive type

data Tree a
    = Empty
    | Node (Tree a) a (Tree a)