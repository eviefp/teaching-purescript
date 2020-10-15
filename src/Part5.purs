module Part5 where

import Prelude

{- Semigroup, append 
   <> = append

   Associativity law:
--------------------------------
   append (append x y) append z = append x (append y z)
   (x <> y) <> z = x <> (y <> z)

Examples:
  - numbers:
   x + y + z
   x * y * z

  - booleans:
   p && q && r
   p || q || r

  - strings:
  "hello" <> "world" <> "!"
    -> "helloworld" <> "1" -> "helloworld1"
    -> "hello" <> "world1" -> "helloworld1"

-}

-- Law: Associativity: (x <> y) <> z = x <> (y <> z)
class Semigroup' a where
  append' :: a -> a -> a

instance semigroupString :: Semigroup' String where
  append' s1 s2 = s1 <> s2


data ListInt
    = Empty
    | MkList Int ListInt

instance semigroupListInt :: Semigroup' ListInt where
  append' Empty l2 = l2
  append' (MkList int1 listint1) list2 = 
           MkList int1 (append' listint1 list2)


{-

    Left unit: (mempty <> x) = x
    Right unit: (x <> mempty) = x

-}

class Semigroup' a <= Monoid' a where
  mempty' :: a

instance monoidString :: Monoid' String where
  mempty' = ""

instance monoidListInt :: Monoid' ListInt where
   mempty' = Empty

-- 
data Add = Add Int

instance semigroupAdd :: Semigroup' Add where
  append' (Add a1) (Add a2) = Add (a1 + a2)

instance monoidAdd :: Monoid' Add where
  mempty' = Add 0

data Mul = Mul Int

instance semigroupMul :: Semigroup' Mul where
  append' (Mul a1) (Mul a2) = Mul (a1 * a2)

instance monoidMul :: Monoid' Mul where
  mempty' = Mul 1

---------------------------------------------------------------------

{-
  add = +
  mul = *
-}
class Semiring' a where
  add' :: a -> a -> a
  zero' :: a
  mul' :: a -> a -> a
  one' :: a

instance semiringInt :: Semiring' Int where
  add' :: Int -> Int -> Int
  add' x y = x + y
  zero' = 0
  mul' x y = x * y
  one' = 1

instance semiringBoolean :: Semiring' Boolean where
  add' x y = x || y
  zero' = false
  mul' x y = x && y
  one' = true

-- sub = -
class Semiring' a <= Ring' a where
  sub :: a -> a -> a

instance ringInt :: Ring' Int where
  sub x y = x - y


class Ord a <= Bounded' a where
  top' :: a
  bottom' :: a

instance boundedBoolean :: Bounded' Boolean where
  top' = true
  bottom' = false


{- Laws
----------------------------------------------

  Associativity:
  
      a || (b || c) = (a || b) || c
      a && (b && c) = (a && b) && c
  
  Commutativity:
  
      a || b = b || a
      a && b = b && a
  
  Absorption:
  
      a || (a && b) = a
      a && (a || b) = a
  
  Idempotent:
  
      a || a = a
      a && a = a
  
  Identity:
  
      a || ff = a
      a && tt = a
  
  Implication:
  
      a `implies` a = tt
      a && (a `implies` b) = a && b
      b && (a `implies` b) = b
      a `implies` (b && c) = (a `implies` b) && (a `implies` c)
  
  Complemented:
  
      not a = a `implies` ff
  
-}
class HeytingAlgebra' a where
  ff :: a
  tt :: a
  implies :: a -> a -> a
  conj :: a -> a -> a
  disj :: a -> a -> a
  not :: a -> a

instance heytingAlgebraBoolean :: HeytingAlgebra' Boolean where
  ff = false
  tt = true
  implies x y
    | x == true && y == false = false 
    | otherwise = true
  conj x y = x && y
  disj x y = x || y
  not x
    | x == true = false
    | otherwise = true 


--------------------------------------------------------
-- Polymorphic functions

{-
We usually write functions like

f1 :: Int -> Int
f1 i = i + 1

But the idea is, when we use _Concrete_ types (like Int, String, ListInt, etc.), there are a lot
of operations that are available on them.

On the hand, if we have a _Polymorphic_ type (introduced via the 'forall' keyword), we know NOTHING
about it, so we can't apply ANY operation on them (other than polymorphic operations).
-}

-- for any type 'a', this is a function that takes a value of that type as input
-- and returns a value of that type
-- And it has to work for ANY POSSIBLE TYPE.
f1 :: forall a. a -> a
-- Now, if I want to type:
-- > f1 true
-- then the compiler will look at `f` and `true` and match them together:
-- f1 :: Boolean -> Boolean (because `true :: Boolean` and f1 is polymorphic)
--
-- Since we know NOTHING about 'a', the only possible thing we can do is just return it unchanged
f1 a1 = a1

-- In this example, we can pick either the first or the second input. We cannot "smash" them togheter
-- for example, using Semigroup because we don't know that there exists a semigroup instance for 'a'
f2 :: forall a. a -> a -> a
f2 a1 a2 = a1

f2' :: forall a. a -> a -> a
f2' a1 a2 = a2

-- There is no other possible implementation. We cannot use 'b' in any way.
f3 :: forall a b. a -> b -> a
f3 a b = a

{- This is not possible to implement.
f4 :: forall a b. a -> b
f4 a = 
-}

{- This is a lot like tetris, but with types:
--------

a   :: a
a2b :: a -> b
-------------------
a2b a :: b
-}
f5 :: forall a b. a -> (a -> b) -> b
f5 a a2b = a2b a

f6 :: forall a b c. (a -> b) -> (b -> c) -> a -> c
f6 a2b b2c a =
  let b = a2b a
  in b2c b

f7 :: forall a b c d. (a -> b) -> (b -> c) -> (c -> d) -> a -> d
f7 a2b b2c c2d a = 
  let b = a2b a
      c = b2c b
      d = c2d c
  in d

f8 :: forall a b c. (a -> b -> c) -> a -> b -> c
f8 a2b2c a b = a2b2c a b

f9 :: forall a b c. (a -> b -> c) -> (a -> b) -> a -> c
f9 a2b2c a2b a =
  let b = a2b a
      c = a2b2c a b
  in c









  -- Homework

  -- 1. Define a new type for And and write Semigroup and Monoid instance
data And = And Boolean

-- It is not possible to define two instances for the same class and type combination.
instance semigroupAnd :: Semigroup' And where
   append' (And s1) (And s2) = And (s1 && s2)

instance monoidAnd :: Monoid' And where
  mempty' = And true

-- for any And a, we want it to be true that:
-- a <> mempty' = a

  -- 2. Define a new type for Or and write Semigroup and Monoid instance

data Or = Or Boolean

instance semigroupOr :: Semigroup' Or where
   append' (Or s1) (Or s2) = Or (s1 || s2)

instance monoidOr :: Monoid' Or where
  mempty' = Or false

-------------------------------------------------------

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

  -- 3. Define Eq instance for Weekday
  -- instance eqWeekday :: Eq Weekday where
  --  eq _ _ = ???

instance eqWeekday :: Eq Weekday where
  -- eq Mon Mon = true
  -- eq Tue Tue = true
  -- ...
  -- eq _ _ = false
  eq a b = toInt a == toInt b

  -- 4. Define Ord instance for Weekday. The possible values to return are
  --   EQ, LT, GT (instead of Equals, Lower, Greater)
  -- instance ordWeekday :: Ord Weekday where
  --   compare _ _ = ???

instance weekdayOrd :: Ord Weekday where
  -- = compare (toInt a) (toInt b)
  compare a b
    | a == b = EQ
    | toInt a > toInt b = GT
    | otherwise = LT 

  -- 5. Define Bounded instance for Weekday.
instance boundedWeekday :: Bounded' Weekday where
  top' = Sun
  bottom' = Mon

  -- 6. Implement the following functions:
  -- Important: Usually, we want to try to "use" all our inputs when we write functions.

h1 :: forall a b c d. (a -> b -> c) -> (c -> d) -> (a -> b) -> a -> d
h1 a2b2c c2d a2b a =
  let b = a2b a
      c = a2b2c a b
      d = c2d c
  in d

h2 :: forall a b c d. (a -> b -> c -> d) -> (a -> b) -> (a -> c) -> a -> d
h2 a2b2c2d a2b a2c a = 
 let b = a2b a
     c = a2c a
     d = a2b2c2d a b c
 in d

h3 :: forall a b c. a -> b -> (a -> b) -> (b -> b -> c) -> c
h3 a b a2b b2b2c =
 let b' = a2b a
     c = b2b2c b' b
 in c

-- a few examples:
monoidal :: forall a b. Monoid a => (a -> b) -> b
monoidal a2b = a2b mempty

increment :: forall a. Semiring a => a -> a
increment a = a + one

--------------

multiConcat :: forall a. Semigroup a => a -> a -> a -> a
multiConcat a1 a2 a3 = a1 <> a2 <> a3


combine :: forall a b. Semigroup b => a -> a -> (a -> b) -> b
combine a1 a2 a2b = 
 let b1 = a2b a1
     b2 = a2b a2
  in b1 <> b2

combine' :: forall a b. Semigroup b => a -> a -> (a -> b) -> b
combine' a1 a2 a2b = a2b a1 <> a2b a2


