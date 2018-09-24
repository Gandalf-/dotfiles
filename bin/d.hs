module Main where

import Apocrypha.Network

import System.Environment (getArgs)

display Nothing  = return ()
display (Just s) = putStrLn s

main :: IO ()
main = do
    context <- getContext Nothing
    getArgs >>= client context >>= display
