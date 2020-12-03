module Part8 where

import Prelude

import Data.Array (fold, head, index)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding as Encoding
import Node.FS.Sync (readTextFile, writeTextFile, rename, unlink)
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
    , from2 :: String
    , to2 :: String
    }

copy' :: Effect Unit
copy' = do
    json <- readTextFile Encoding.ASCII "file.json"
    case (readJSON json :: _ FileFormat) of
        Left _ -> log "invalid json format"
        Right { from, to } -> copyFile from to
    
main :: Effect Unit
main = copyArray

------ Homework:

-- PS: this is how you run:
-- spago bundle-app -m Part8 --to test.js
-- node test.js

-- add two more fields to `file.json`: from2 and to2
-- and implement a function "copy2files" which does what
-- copy' does, but for both sets of files
copy5 :: Effect Unit
copy5 = do
  json <- readTextFile Encoding.ASCII "file.json"
  case (readJSON json :: _ FileFormat) of
      Left _ -> log "invalid json format"
      Right { from, to, from2, to2 } -> do
        copyFile from to
        copyFile from2 to2

-- write a new version of copy, "copyPrompt" which:
-- when the program starts, it asks for a file name (using `question`)
-- then it asks for a second file name, then copies first to second
-- after that is done, it will stop

copyPrompt :: Effect Unit
copyPrompt = do
    interface <- createConsoleInterface noCompletion
    question "Where do I Copy from?" (handleSource interface) interface
  where
    handleSource :: Interface -> String -> Effect Unit
    handleSource interface source = do
        log $ "copy from " <> source
        question "where do I copy to?" (handleDestination interface source) interface

    handleDestination :: Interface -> String -> String -> Effect Unit
    handleDestination interface source destination = do
        log $ "copy to" <> destination
        copyFile source destination
        close interface


-- create a new type like `FileFormat` which holds
-- an array of two string "to" and "from"
-- and copies all pairs
-- the json looks like file2.json

type SimpleFileCopy =
    { to :: String
    , from :: String
    }

type FileFormatJunior =
    { files :: Array SimpleFileCopy
    }

simpleFileCopy :: SimpleFileCopy -> Effect Unit
simpleFileCopy { to, from } = copyFile from to

copyArray :: Effect Unit
copyArray = do
  json <- readTextFile Encoding.ASCII "file2.json"
  case (readJSON json :: _ FileFormatJunior) of
      Left _ -> log "invalid json format"
      Right { files } -> do
         -- files :: Array SimpleFileCopy
         -- simpleFileCopy :: SimpleFileCopy -> Effect Unit
         -- IF we were doing C#, we could do something like:
         -- foreach (f in files) simpleFileCopy(f)
         -- map :: Functor f => (a -> b) -> f a -> f b
         -- map simpleFileCopy files
         ----------------------------
         -- a = SimpleFileCopy
         -- b = Effect Unit
         -- f = Array
         ----------------------------
         -- map :: (SimpleFileCopy -> Effect Unit) -> Array SimpleFileCopy -> Array (Effect Unit)
         -- sequence :: Array (Effect a) -> Effect (Array a)
        fold $ map simpleFileCopy files

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
data Action = C | R | D | Unknown

programNew :: Effect Unit
programNew = do
    interface <- createConsoleInterface noCompletion
    question "Give me the file name, please!" (handleSource interface) interface
  where
    handleSource :: Interface -> String -> Effect Unit
    handleSource i source = do
        log $ "The file name is " <> source
        question "I need a c/r/d action!" (action i source) i

    action :: Interface -> String -> String -> Effect Unit
    action i source str = do
        log $ "The action is " <> str
        let a = checkAction str
        performAction i source a

    performAction :: Interface -> String -> Action -> Effect Unit
    performAction interface source a = 
        case a of 
            C -> question "Destination ?" (handleCopy interface source) interface 
            R -> question "New name? " (handleRename interface source) interface
            D -> question "Are you sure? " (handleDelete interface source) interface 
            Unknown -> do
                log "Unknown option. Restarting..."
                close interface
                programNew

    handleDelete :: Interface -> String -> String -> Effect Unit
    handleDelete i source answer = 
            if answer == "y"
                then do
                    unlink source
                    close i
                else do
                    log "expected 'y'; restarting..."
                    close i
                    programNew
    
    handleRename :: Interface -> String -> String -> Effect Unit
    handleRename i source dest = do
      rename source dest
      close i

    handleCopy :: Interface -> String -> String -> Effect Unit
    handleCopy i source dest = do
        copyFile source dest
        close i

    checkAction :: String -> Action
    checkAction a =
        case a of
            "c" -> C
            "r" -> R
            "d" -> D
            _ -> Unknown

-- Pick a file: "file.json"
-- What to do c/r/d: c
-- Copy! Where to? "file-copy.json"
-- DONE! and exits