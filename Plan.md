# Done

1. Lambda Calculus
2. Types
3. Recursive Types & Functions
4. Typeclasses + Records
5. Semigroup & Monoids & More (numerics & booleans) & polymorphic functions
6. HKT + Functors
7. Applicative & Monad
8. Effects
9. Start building a real program
...

foldable & traversable

- function instances for Semigroup, Monoid, etc.
- reader monad

# Now

- either monad transformer
- improve the application



# Future

In no particular order:
- writer, state monads
- Typeclass counter examples
- parsers
- monad transformers
- variant, run, ... cool libraries
- curry-howard


## Application Plan

Link: https://www.coingecko.com/en/api#explore-api

Settings:
- API link
- recurrence (how often to call it)
- list of per-coin settings:
    - coin id
    - high value alert threshold
    - low value alert threshold

Polls an API to grab current values for a set of coins:
- get data from API
- checks if current value is below or over thresholds
- alerts if yes (just console logging)

https://pursuit.purescript.org/packages/purescript-affjax/11.0.0