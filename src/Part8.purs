module Part8 where

import Prelude

import Data.Array (head, index)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding as Encoding
import Node.FS.Sync (readTextFile, writeTextFile)
import Node.Process (argv)
import Node.ReadLine (Interface, close, createConsoleInterface, noCompletion, question)
import Simple.JSON (readJSON)
import Test.QuickCheck.Gen (chooseInt, randomSample)

-- Purescript & Haskell are Pure Typed Functional programming languages
-- We work with functions, types, etc.

-- We never talked about: variables, reading or writing to console or files, SQL, web, etc.

-- Effect :: Type -> Type
-- Effect Int
-- Effect Unit
-- Effect (Maybe Int)

greet :: Effect Unit
greet = log "Hello, world"

input :: Effect Unit
input = do
    interface <- createConsoleInterface noCompletion
    let
        gen = chooseInt 1 1000
    array <- randomSample gen
    log $ show array
    let
        number = case head array of
            Nothing -> 42
            Just value -> value
    question "Pick a number: " (handler interface number) interface
  where
    handler :: Interface -> Int -> String -> Effect Unit
    handler interface num str = do
        -- check if input is equal to number
        let mCompareResult = checkInput num str

        case mCompareResult of
            Just GT -> do
                log "Greater!"
                question "Pick another thing: " (handler interface num) interface
            Just EQ -> do
                log "Congratulations!"
                close interface -- or just 'exit 0'
            Just LT -> do
                log "Lower!"
                question "Pick another thing: " (handler interface num) interface
            Nothing -> do
                log "Not a number!"
                question "Pick another thing: " (handler interface num) interface
    
    -- Just GT, Just EQ, Just LT, Nothing
    checkInput :: Int -> String -> Maybe Ordering
    checkInput num str =
        let guess = Int.fromString str
        in case guess of
            Just guess' -> Just $ compare num guess'
            Nothing     -> Nothing

copyFile :: String -> String -> Effect Unit
copyFile source dest = do
    file <- readTextFile Encoding.ASCII source
    writeTextFile Encoding.ASCII dest file

copy :: Effect Unit
copy = do
    arguments <- argv
    case index arguments 2, index arguments 3 of
        Just s1, Just s2 -> copyFile s1 s2
        _, _             -> log "copy <source> <dest>"

type FileFormat =
    { from :: String
    , to   :: String
    }

copy' :: Effect Unit
copy' = do
    json <- readTextFile Encoding.ASCII "file.json"
    case (readJSON json :: _ FileFormat) of
        Left _ -> log "invalid json format"
        Right { from, to } -> copyFile from to
    
main :: Effect Unit
main = copy'

------ Homework:

-- PS: this is how you run:
-- spago bundle-app -m Part8 --to test.js
-- node test.js

-- add two more fields to `file.json`: from2 and to2
-- and implement a function "copy2files" which does what
-- copy' does, but for both sets of files

-- write a new version of copy, "copyPrompt" which:
-- when the program starts, it asks for a file name (using `question`)
-- then it asks for a second file name, then copies first to second
-- after that is done, it will stop

-- create a new type like `FileFormat` which holds
-- an array of two string "to" and "from"
-- and copies all pairs
-- the json looks like file2.json

-- https://pursuit.purescript.org/packages/purescript-node-fs/5.0.1/docs/Node.FS.Sync
-- write a program that uses `question` to allow users to:
-- first, select a file name
-- then, select an action such as:
-- copy, rename, or delete
-- for copy, ask for destination
-- for rename, ask for new name
-- for delete, ask for confirmation
-- for example, when you start the program:
--
-- Pick a file: "file.json"
-- What to do c/r/d: c
-- Copy! Where to? "file-copy.json"
-- DONE! and exits