module Main where

import Apocrypha.Network

import Data.Maybe (fromMaybe)
import System.Environment (getArgs)


main :: IO ()
main = do
    context <- getContext Nothing
    getArgs >>= client context >>= putStrLn . fromMaybe ""
