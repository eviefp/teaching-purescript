module Part9 where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Node.FS.Sync (rename, unlink)
import Node.ReadLine (Interface, createConsoleInterface, noCompletion)
import Node.ReadLine.Aff (close, question)
import Part8 (copyFile)

main :: Effect Unit
main = do
    interface <- createConsoleInterface noCompletion
    launchAff_ do
        programNew interface
        close interface

data Action = C | R | D | Unknown

programNew :: Interface -> Aff Unit
programNew interface = do
    source <- question "Give me the file name, please!" interface
    log $ "The file name is " <> source
    action <- question "I need a c/r/d action!" interface
    log $ "The action is " <> action
    let a = checkAction action
    case a of 
        C -> do
            dest <- question "Destination ?" interface 
            liftEffect $ copyFile source dest
        R -> do
            newName <- question "New name? " interface
            liftEffect $ rename source newName
            pure unit
        D -> do
            answer <- question "Are you sure? " interface 
            if answer == "y"
                then do
                    liftEffect $ unlink source
                else do
                    log "expected 'y'; restarting..."
                    programNew interface
        Unknown -> do
            log "Unknown option. Restarting..."
            programNew interface
  where
    checkAction :: String -> Action
    checkAction a =
        case a of
            "c" -> C
            "r" -> R
            "d" -> D
            _ -> Unknown
