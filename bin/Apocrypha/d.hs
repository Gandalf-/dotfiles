module Main where

import           Apocrypha.Network  (client, getContext)
import           System.Environment (getArgs)

display Nothing  = return ()
display (Just s) =
        if null s || s == "\n"
            then return ()
            else putStr s

main :: IO ()
main = do
        c <- getContext Nothing
        getArgs >>= client c >>= display
