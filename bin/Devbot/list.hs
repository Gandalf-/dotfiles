module Main where

import Common.ColorText
import Devbot.Core

import Data.List (intercalate)
import Data.Time.Clock.POSIX (getPOSIXTime)

now :: IO Integer
now = round `fmap` getPOSIXTime

blue   = (Blue, NoColor, Null) :: Decoration
black  = (White, NoColor, Null) :: Decoration
green  = (Green, NoColor, Bold) :: Decoration
yellow = (Yellow, NoColor, Null) :: Decoration
red    = (Red, NoColor, Null) :: Decoration
cyan   = (Cyan, NoColor, Null) :: Decoration

printAction :: Config -> String
printAction (Config act _ _ )  = decorate a blue
    where a    = "    " ++ intercalate pad act
          pad  = "\n    "


printName :: String -> String
printName name = decorate name green


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


printNext :: Data -> Integer -> String
printNext (Data _ w _) time
    | w - time > 0 = decorate ("next in " ++ t) yellow
    | otherwise    = decorate "now" yellow
    where t      = prettyTime $ w - time


printOptional :: Config -> Data -> String
printOptional (Config _ _ req) (Data d _ errs) =
        concat [ printErrors errs
               , printDuration d
               , printRequire req
               ]
    where
          printErrors :: Maybe Integer -> String
          printErrors Nothing = ""
          printErrors (Just s) = ", " ++ decorate (show s ++ " errors") red

          printRequire :: Maybe String -> String
          printRequire Nothing = ""
          printRequire (Just r') = ", requires " ++ r'

          printDuration :: Integer -> String
          printDuration s =
                ", " ++ decorate ("took " ++ show s ++ " seconds") cyan


printEvent :: Event -> IO ()
printEvent (Event n c d) = do
    time <- now
    putStrLn $ concat
        [ printName n, "\n"
        , printAction c, "\n"
        , printInterval c
        , printOptional c d, ", "
        , printNext d time
        , "\n"
        ]

main :: IO ()
main = events >>= mapM_ printEvent
