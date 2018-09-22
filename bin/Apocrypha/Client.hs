{-# LANGUAGE FlexibleInstances #-}

module Apocrypha.Client
    ( keys
    , get
    , getBytes
    , set
    , del
    , getContext
    , cleanContext
    , Context
    ) where

import Data.Aeson
import Apocrypha.Network

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as B

getBytes = jClient


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
    m <- getBytes context $ items ++ ["--edit"]
    return (Data.Aeson.decode m :: (FromJSON a) => Maybe a)
