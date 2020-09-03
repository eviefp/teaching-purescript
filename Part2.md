# Types

## Recap
Done in Examples1.

## What are types

Types are always capitalized. Functions always start with a lowercase letter.

Values always have types denoted by `value :: Type`.

For example:
- "a" :: String, "abc" :: String, ...
- 1 :: Int, 2 :: Int, ...
- 1.0 :: Number, 1.5 :: Number, ...
- true :: Boolean, false :: Boolean

```purescript
it :: Boolean
it = false
```

## Types as Sets

We can consider `Boolean` as the boolean set along with its values.

```
B = { true, false }
Z = { -inf, -9999999, ..., 0, 1, ... +inf }
S = { "", "a", "aa", ..., "ab", ... }
```

The cardinality of a set is the number of elements of that set.

```
|B| = 2
|Z| = inf
Weekdays = { mon, tue, wed, thu, fri, sat, sun }
|Weekdays| = 7
```

## Defining our own wrappers

When we write:
```purescript
data MyBools = CreateMyBools Boolean
```
- `data` is a keyword
- `MyBools` is a new type that we just created
- `CreateMyBools` is a data constructor for `MyBools`
- `CreateMyBools :: Boolean -> MyBools`

Then, we have the following new values:
- `CreateMyBools true :: MyBools`, `CreateMyBools false :: MyBools`

`|Mybools| = 2`

## Pattern matching

```purescript
-- if first is "hello", then return "hello"
-- else if condition then first, else second
cond' :: Boolean -> String -> String -> String
cond' b     "hello" s2 = "hello"
cond' true  s1      s2 = s1
cond' false s1      s2 = s2
```

## Products

Let's start with an example: points. A point is a coordinate on a two-dimmensional plane. For example: (0, 0), (1, 5), etc.

```purescript
data Point = MkPoint Int Int
```
- `Point` is a new type that we just created
- `MkPoint` is a data constructor for `Point`
- `MkPoint :: Int -> Int -> Point`

Then we can create the following values:
- `MkPoint 0 0` which would be like (0, 0)
- `MkPoint 1 5` for (1, 5), etc.

```purescript
data TwoBits = MkBits Boolean Boolean
data ThreeBits = MkBits Boolean Boolean Boolean
```

```
|TwoBits| = 4 (TT TF FT FF)
|ThreeBits| = 2 * 2 * 2 = 8
```

These types are called 'product' types. So the cardinality of such a type is an algebric product between its 'components'.

## Functions

Functions are a type written as `a -> b` where `a` and `b` are types. We can
write things like `Int -> String` or `String -> String` etc.

We can also write functions with multiple parameters like `Int -> Int -> Int`, `String -> String -> String`, etc.

The trick here is that multiple parameter functions are actually encoded as
`Int -> (Int -> Int)`.

Let's consider
```purescript
it :: Int -> Int -> Int -> Int
it x y z = x + y + z
```
- `it :: Int -> Int -> Int -> Int`
- `it 1 :: Int -> Int -> Int`
- `it 1 2 :: Int -> Int`
- `it 1 2 3 :: Int`
- `it 1 2 3 4 :: type/compilation error`


```purescript
f :: Int -> Int -> Int
f x y = x + y + 1
```

`f 1 2 =>>> 4`


Cardinality of functions.

The cardinality of functions like `Boolean -> Boolean` is:
```purescript
fn1 :: Boolean -> Boolean
fn1 true = true
fn1 false = false

fn2 :: Boolean -> Boolean
fn2 true = false
fn2 false = true

fn3 :: Boolean -> Boolean
fn3 true = true
fn3 false = true

fn4 :: Boolean -> Boolean
fn4 true = false
fn4 false = false
```

| name    | output for T | output for F |
| id      | true         | false        |
| not     | false        | true         |
| const T | true         | true         |
| const F | false        | false        |

`Boolean -> Boolean -> Boolean`

| name    | FF | FT | TF | TT |
| const T | T  | T  | T  | T  |
| const F | F  | F  | F  | F  |
|           T    F    F    F
            F    T    F    F
            F    F    T    F
...
| and     | F    F    F    T
| or      | F    T    T    T
| xor ...
| ->
| not or |  T    F    F    F


2^(2 * 2)

The cardinality of a function is an exponential, so for `a -> b` the cardinality is `b ^ a`.


## Sums

```purescript
data Weekday
    = Mon | Tue | Wed | Thu | Fri | Sat | Sun
```
- `Weekday` is a new type that we just created
- `Mon`, `Tue`, ..., `Sun` are *ALL* new constructors
- `Mon :: Weekday`, `Tue :: Weekday`, ..., `Sun :: Weekday`

```purescript
monday :: Weekday
monday = Mon

--- option 1
toInt :: Weekday -> Int
toInt Mon = 1
toInt Tue = 2
...
toInt Sun = 7
```

```purescript
data PointT
    = Point2D Int Int
    | Point3D Int Int Int
```
- `PointT` is a new type that we just created
- `Point2D :: Int -> Int -> PointT`
- `Point3D :: Int -> Int -> Int -> PointT`
- `Point2D 1 2 :: PointT` `Point3D 1 2 3 :: PointT`

```purescript
data AlternatingWeek
    = First Weekday
    | Second Weekday
```
- `AlternatingWeek` is a new type that we just created
- `First, Second :: Weekday -> AlternatingWeek`
- `First Mon`, `Second Tue`

```purescript
f :: AlternatingWeek
f = First Mon

isFirst :: AlternatingWeek -> Boolean
isFirst (First _) = true
isFirst _         = false
```

Cardinality for sum types:
- First Mon, First Tue, ..., First Sun    = 7
- Second Mon, Second Tue, ..., Second Sun = 7

The cardinality of sum types is obtained by adding together their 'components', so in this case `|Weekday| + |Weekday| = 7 + 7 = 14`.

## Type notation

`a * b` => it's a product type, so something like `data Product = MkProduct a b`
`b ^ a` => it's a function type, so something like `a -> b`
`a + b` => it's a sum type, so something like `data Sum = C1 a | C2 b`

## Exercises:

1. Define a type to hold information about a user: a username (as string),
an email (also string), and age (as int).
2. Write functions to extract each property, e.g. `getUsername :: User -> String`
3. Write a function to increment (+1) the age of a user `incrementAge :: User -> User`
4. Write a function that returns the oldest user `oldest :: User -> User -> User`
5. Write a new type `Person` that has the following constructors:
    - `Employee` with a `Boolean` property which represents whether he is a new employee
    - `Manager` with two `Boolean` properties: whether he is also an admin, and whether they have their own office
6. Write a function `Person -> Boolean` which returns true if the person is an employee.
7. Write a function `Person -> Boolean` which returns true if the person is a manager AND they are an admin AND they have their own office.
8. Write a function `Person -> Person` which makes the `Manager` an admin regardless of what they were before, and leaves the `Employee` as before.
9. Given:
```purescript
data One = OOne
data Three = TOne | TTwo | TThree
```
What's the cardinality for:
```purescript
data T1 = C1 Three Boolean
data T2 = C2 T1 Three
data T3 = C3 Three Three
data T4 = C4 T2 T3

data T5 = C51 Boolean | C52 T4
data T6 = C61 T1 | C62 T2 | C63 T4 | C64 T5

data T7 = C7 (One -> One)
data T8 = C8 (Three -> Boolean)
data T9 = C9 (Three -> Three)
data T10 = C10 (T2 -> T4)

data T11 = C11_1 One | C11_2 (T1 -> T2) | C11_3 (T2 -> T4) T8
```