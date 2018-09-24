module Main where

import Apocrypha.Client (keys)
import System.Directory (getHomeDirectory, doesFileExist)
import System.Exit (ExitCode(..))
import System.Process (spawnCommand, waitForProcess)

home :: IO String
home = getHomeDirectory

pfile = (++ "/.devbot/pid") <$> home
lfile = (++ "/.devbot/log") <$> home

data Status = Stopped | Running | Mystery | Database

status :: Status -> IO ()
status Running  = putStrLn "✓"
status Stopped  = putStrLn "✗"
status Mystery  = putStrLn "?"
status Database = putStrLn "!"

alive :: IO Bool
alive = (not . null) <$> keys Nothing ["devbot"]

checkRunning :: IO ()
checkRunning = do
    pid  <- pfile >>= readFile
    h    <- spawnCommand $ "kill -0 " ++ pid
    code <- waitForProcess h

    case code of
        ExitSuccess -> status Running
        _           -> status Mystery

checkStarted :: IO ()
checkStarted = do
    pExists <- pfile >>= doesFileExist

    if pExists
        then checkRunning
        else status Stopped

main :: IO ()
main = do
    databaseAlive <- alive

    if databaseAlive
        then checkStarted
        else status Database
