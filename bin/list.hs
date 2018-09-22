module Main where

import Common.ColorText
import Devbot.Core

import Data.List (intercalate)
import Data.Time.Clock.POSIX (getPOSIXTime)

now :: IO Integer
now = round `fmap` getPOSIXTime

printAction :: Config -> String
printAction (Config act _ _ )  = decorate a blue
    where a    = "    " ++ intercalate pad act
          blue = (Blue, Black, Null) :: Decoration
          pad  = "\n    "

printName :: String -> String
printName name = decorate name green
    where green = (Green, Black, Bold) :: Decoration

prettyTime :: Integer -> String
prettyTime i
    | i <= minute = show i ++ " seconds"
    | i <= hour   = show (div i minute) ++ " minutes"
    | i <= day    = show (div i hour) ++ " hours"
    | otherwise   = show (div i day) ++ " days"
    where day = 86400
          hour = 3600
          minute = 60

printInterval :: Config -> String
printInterval (Config _ i _) =
        decorate ("    every " ++ prettyTime i) cyan
    where cyan = (Cyan, Black, Null) :: Decoration

printNext :: Data -> Integer -> String
printNext (Data _ w _) time
    | w - time > 0 = decorate ("next in " ++ t) yellow
    | otherwise    = decorate "now" yellow
    where t      = prettyTime $ w - time
          yellow = (Yellow, Black, Null)


printOptional :: Config -> Data -> IO ()
printOptional (Config _ _ req) (Data _ _ errs) = do
    printErrors errs
    printRequire req
    putStrLn ""
    where
          printErrors :: Maybe Integer -> IO ()
          printErrors Nothing = return ()
          printErrors (Just s) =
                putStr $ ", " ++ decorate (show s ++ " errors") red

          printRequire :: Maybe String -> IO ()
          printRequire Nothing = return ()
          printRequire (Just r') =
              putStr $ ", requires " ++ r'

          red = (Red, Black, Null) :: Decoration

printEvent :: Event -> IO ()
printEvent (Event n c d) = do
    putStrLn $ printName n
    putStrLn $ printAction c
    putStr $ printInterval c
    time <- now
    putStr $ ", " ++ printNext d time
    printOptional c d
    putStrLn ""

main :: IO ()
main = events >>= mapM_ printEvent
