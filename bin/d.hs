module Main where

import Apocrypha.Network

import Data.Foldable (forM_)
import System.Environment (getArgs)


main :: IO ()
main = do
    args <- getArgs
    context <- getContext
    result <- client context args
    forM_ result putStrLn
