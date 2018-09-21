{-# LANGUAGE FlexibleInstances #-}

module Apocrypha.Client
    ( keys, keys'
    , get, get'
    , jGet
    , set, set'
    , del
    , getter
    , getContext
    , Context
    ) where

import Data.Aeson
import Apocrypha.Network

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as B

keys :: Context -> [String] -> IO [String]
keys con items = do
    result <- client con $ items ++ ["--keys"]
    return $ maybe [] words result

get :: Context -> [String] -> IO (Maybe String)
get  = client
jGet = jClient

del :: Context -> [String] -> IO ()
del con items = do
    _ <- client con $ items ++ ["--del"]
    return ()

-- create a new socket every time versions
keys' :: [String] -> IO [String]
keys' items = do
    result <- client Nothing $ items ++ ["--keys"]
    return $ maybe [] words result

get' :: [String] -> IO (Maybe String)
get' = client Nothing

set' :: [String] -> String -> IO (Maybe String)
set' items value = client Nothing $ items ++ ["=", value]

set :: (ToJSON a) => Context -> [String] -> a -> IO ()
set context items value = do
    _ <- client context $ items ++ ["--set", v]
    return ()
    where v = B8.unpack . B.toStrict . encode $ value

getter :: (FromJSON a) => Context -> [String] -> IO (Maybe a)
getter context items = do
    m <- jGet context $ items ++ ["--edit"]
    return (Data.Aeson.decode m :: (FromJSON a) => Maybe a)
