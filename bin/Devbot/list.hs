module Main where

import           Common.ColorText
import           Devbot.Core

import           Data.List             (intercalate)
import           Data.Time.Clock.POSIX (getPOSIXTime)

now :: IO Integer
now = round `fmap` getPOSIXTime

blue   = (Blue,   NoColor, Null)
black  = (White,  NoColor, Null)
green  = (Green,  NoColor, Bold)
yellow = (Yellow, NoColor, Null)
red    = (Red,    NoColor, Null)
cyan   = (Cyan,   NoColor, Null)


printAction :: Config -> String
printAction (Config a _ _ )  = decorate action blue
    where action = "    " ++ intercalate pad a
          pad    = "\n    "


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
        concat [ maybe "" printErrors errs
               , printDuration d
               , maybe "" printRequire req
               ]
    where
          printErrors :: Integer -> String
          printErrors e = ", " ++ decorate (show e ++ " errors") red

          printDuration :: Integer -> String
          printDuration d =
                ", " ++ decorate ("took " ++ show d ++ " seconds") cyan

          printRequire :: String -> String
          printRequire r = ", requires " ++ r


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
