# What is a Monad?

All monads produce 0 to n values of some type. Given monad `m`, whenever we have some
`m a`, then the `a` is that value's type. But this is actually true for any functor.

Any functor `f a` has a "positive" `a`.

`a -> b` are also functors (and monads), and while the may eventually produce some `a`,
they don't provide one "directly".

- it's like a burrito
- it's like a pipe (c) Your Least Favorite Racist Uncle
- it's like a container
- it's like ...

DO NOT EXPLAIN BY ANALOGY!

A monad is a
   *higher kinded type*
   with two operations:

```haskell
bind :: m a -> (a -> m b) -> m b

pure :: a -> m a
```


```math
join : m (m a) -> m a
pure : a -> m a
```


```haskell
getLine :: IO String
read :: String -> Int

read <$> getLine
```



## What is a Higher kinded type

Int, String, Bool are concrete types

List, Maybe (Option), Either, etc., are Higher Kinded Types (type constructors)


List a :: Type
List Int :: Type -- Concrete
List :: Type -> Type -- unsaturated

Either :: Type -> Type -> Type
Either String :: Type -> Type
Either String Int :: Type