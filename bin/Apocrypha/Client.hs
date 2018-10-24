{-# LANGUAGE FlexibleInstances #-}

module Apocrypha.Client
    ( keys,  get,  set,  del , pop , append
    , keys', get', set', del', pop', append'
    , Context, getContext
    ) where

import           Apocrypha.Network
import           Data.Aeson

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy  as B


keys :: Context -> [String] -> IO [String]
keys c items = do
    result <- client c $ items ++ ["--keys"]
    return $ maybe [] words result

keys' items = do
    c <- getContext Nothing
    keys c items

del :: Context -> [String] -> IO ()
del con items = do
    _ <- client con $ items ++ ["--del"]
    return ()

del' items = do
    c <- getContext Nothing
    del c items


set :: (ToJSON a) => Context -> [String] -> a -> IO ()
set context items value = do
    _ <- client context $ items ++ ["--set", v]
    return ()
    where v = B8.unpack . B.toStrict . encode $ value

set' items value = do
    c <- getContext Nothing
    set c items value


get :: (FromJSON a) => Context -> [String] -> IO (Maybe a)
get context items = do
    result <- jClient context $ items ++ ["--edit"]
    case result of
        Just m  -> return (Data.Aeson.decode m :: (FromJSON a) => Maybe a)
        Nothing -> return Nothing

get' items = do
    c <- getContext Nothing
    get c items


append :: Context -> [String] -> String -> IO ()
append context items value = do
    _ <- client context $ items ++ ["+", value]
    return ()

append' items value = do
    c <- getContext Nothing
    append c items value


pop :: Context -> [String] -> IO (Maybe String)
pop context items =
    client context $ items ++ ["--pop"]

pop' items = do
    c <- getContext Nothing
    pop c items
