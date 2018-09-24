{-# LANGUAGE FlexibleInstances #-}

module Apocrypha.Client
    ( keys,  get,  set,  del , pop , append
    , keys', get', set', del', pop', append'
    , Context, getContext, cleanContext
    ) where

import Data.Aeson
import Apocrypha.Network

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as B


keys :: Context -> [String] -> IO [String]
keys c items = do
    result <- client c $ items ++ ["--keys"]
    return $ maybe [] words result

keys' items = do
    c <- getContext Nothing
    r <- keys c items
    cleanContext c
    return r


del :: Context -> [String] -> IO ()
del con items = do
    _ <- client con $ items ++ ["--del"]
    return ()

del' items = do
    c <- getContext Nothing
    r <- del c items
    cleanContext c
    return r


set :: (ToJSON a) => Context -> [String] -> a -> IO ()
set context items value = do
    _ <- client context $ items ++ ["--set", v]
    return ()
    where v = B8.unpack . B.toStrict . encode $ value

set' items value = do
    c <- getContext Nothing
    r <- set c items value
    cleanContext c
    return r


get :: (FromJSON a) => Context -> [String] -> IO (Maybe a)
get context items = do
    m <- jClient context $ items ++ ["--edit"]
    return (Data.Aeson.decode m :: (FromJSON a) => Maybe a)

get' items = do
    c <- getContext Nothing
    r <- get c items
    cleanContext c
    return r


append :: Context -> [String] -> String -> IO ()
append context items value = do
    _ <- client context $ items ++ ["+", value]
    return ()

append' items value = do
    c <- getContext Nothing
    _ <- append c items value
    cleanContext c


pop :: Context -> [String] -> IO (Maybe String)
pop context items =
    client context $ items ++ ["--pop"]

pop' items = do
    c <- getContext Nothing
    r <- pop c items
    cleanContext c
    return r
