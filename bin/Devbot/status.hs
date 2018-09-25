module Main where

import Apocrypha.Client (keys)
import System.Directory (getHomeDirectory, doesFileExist)
import System.Exit (ExitCode(..))
import System.Process (spawnCommand, waitForProcess)


main :: IO ()
main = do
    databaseAlive <- checkAlive

    if databaseAlive
        then checkStarted
        else status Database

    where
        checkAlive :: IO Bool
        checkAlive = (not . null) <$> keys Nothing ["devbot"]


checkStarted :: IO ()
checkStarted = do
    pExists <- pfile >>= doesFileExist

    if pExists
        then checkRunning
        else status Stopped


checkRunning :: IO ()
checkRunning = do
    pid  <- pfile >>= readFile
    code <- spawnCommand ("kill -0 " ++ pid) >>= waitForProcess

    case code of
        ExitSuccess -> status Running
        _           -> status StalePid


pfile = (++ "/.devbot/pid") <$> getHomeDirectory

data Status = Stopped | Running | StalePid | Database

status Running  = putStrLn "✓"
status Stopped  = putStrLn "✗"
status StalePid = putStrLn "?"
status Database = putStrLn "!"
