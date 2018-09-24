module Main where

import Apocrypha.Client
import System.Directory

home :: IO String
home = getHomeDirectory

pfile = (++ "/.devbot/pid") <$> home
lfile = (++ "/.devbot/log") <$> home

alive :: IO Bool
alive = (not . null) <$> keys' ["devbot"]

checkRunning :: IO ()
checkRunning = undefined

checkStarted :: IO ()
checkStarted = do
    pExists <- pfile >>= doesFileExist

    if pExists
        then checkRunning
        else putStrLn "✗"

main :: IO ()
main = do
    databaseAlive <- alive

    if databaseAlive
        then checkStarted
        else putStrLn "!"
