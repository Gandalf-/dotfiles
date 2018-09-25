{-# LANGUAGE FlexibleInstances #-}

module Apocrypha.Client
    ( keys,  get,  set,  del , pop , append
    , keys', get', set', del', pop', append'
    , Context, getContext, cleanContext
    ) where

import Data.Aeson
import Apocrypha.Network

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy  as B


keys :: Context -> [String] -> IO [String]
keys c items = do
    result <- client c $ items ++ ["--keys"]
    return $ maybe [] words result


del :: Context -> [String] -> IO ()
del con items = do
    _ <- client con $ items ++ ["--del"]
    return ()


set :: (ToJSON a) => Context -> [String] -> a -> IO ()
set context items value = do
    _ <- client context $ items ++ ["--set", v]
    return ()
    where v = B8.unpack . B.toStrict . encode $ value


get :: (FromJSON a) => Context -> [String] -> IO (Maybe a)
get context items = do
    m <- jClient context $ items ++ ["--edit"]
    return (Data.Aeson.decode m :: (FromJSON a) => Maybe a)


append :: Context -> [String] -> String -> IO ()
append context items value = do
    _ <- client context $ items ++ ["+", value]
    return ()


pop :: Context -> [String] -> IO (Maybe String)
pop context items =
    client context $ items ++ ["--pop"]


pop'    = pop    Nothing
keys'   = keys   Nothing
del'    = del    Nothing
append' = append Nothing

get' :: (FromJSON a) => [String] -> IO (Maybe a)
get' = get Nothing

set' :: (ToJSON a) => [String] -> a -> IO ()
set' = set Nothing
