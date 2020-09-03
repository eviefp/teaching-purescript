module Main where

-- This is a comment //
import Prelude

import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)

{- this is a multi-line
comment 
-}

add :: Int -> Int -> Int
add = add_3_numbers 0
-- add = \x -> \y -> x + y

-- add 1
add1 :: Int -> Int
add1 = add_3_numbers 0 1

add5 :: Int -> Int
add5 = add 5
-- add' = \y -> 1 + y

add_3_numbers :: Int -> Int -> Int -> Int
add_3_numbers = \x -> \y -> \z -> x + y + z

add_3_numbers_easy :: Int -> Int -> Int -> Int
add_3_numbers_easy x y z = x + y + z

add1_easy :: Int -> Int
add1_easy x = add_3_numbers 0 1 x

-- "Hello" -> "Hello!"
shout :: String -> String
shout s = s <> "!"

-- 1 -> "1"
showInt :: Int -> String
showInt i = show i

-- 1 2  -> "12"
-- 5 10 -> "510"
concatenate :: Int -> Int -> String
concatenate x y = show x <> show y

-- 5 1 "yes"
-- 1 5 "nope"
isLarger :: Int -> Int -> String
isLarger x y = ifElse (x > y) "yes" "nope"

ifElse :: Boolean -> String -> String -> String
ifElse b ifTrue ifFalse = 
    if b
        then ifTrue
        else ifFalse

addNumbers :: Number -> Number -> Number
addNumbers x y = x + y

{-
String: "a" "abc" "test"
Int: 1 2 3... 100
Boolean: true false
Number: 1.0 1.5 1.55 1.123123121 ...
-}

main :: Effect Unit
main = do
  log "🍝"
  logShow $ (\x -> \y -> x + y) 1 2
  logShow $ add 1 2
  log $ concatenate 5 10
  log $ isLarger 5 1
  log $ isLarger 1 5
  logShow $ addNumbers 1.0 2.5