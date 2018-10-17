module Main where

import           Apocrypha.Network  (client, getContext)
import           System.Environment (getArgs)

display Nothing  = return ()
display (Just s) =
        if null s
            then return ()
            else putStrLn s

main :: IO ()
main = do
        c <- getContext Nothing
        getArgs >>= client c >>= display
