module Main where

import Apocrypha.Network (client)
import System.Environment (getArgs)

display Nothing  = return ()
display (Just s) = putStrLn s

main :: IO ()
main = getArgs >>= client Nothing >>= display
