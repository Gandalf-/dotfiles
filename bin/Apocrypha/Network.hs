module Apocrypha.Network
    ( client, jClient
    , Context, getContext
    , protoSend, protoRead
    ) where

import           Network

import           Control.Exception     (SomeException, try)
import           Data.Binary           (decode, encode)
import           Data.List             (intercalate)
import           Data.Maybe            (fromMaybe)
import           GHC.IO.Handle.Types   (Handle)

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy  as B


type Context = Maybe Handle


getContext :: Maybe (String, PortNumber) -> IO Context
getContext Nothing =
        getContext $ Just ("127.0.0.1", 9999)

getContext (Just (host, port)) = do
        result <- try (connectTo host $ PortNumber port
                      ) :: IO (Either SomeException Handle)
        case result of
            Left _  -> return Nothing
            Right h -> return $ Just h


protoSend :: Handle -> ByteString -> IO ()
protoSend h = B8.hPut h . protocol


protoRead :: Handle -> IO (Maybe ByteString)
protoRead h = do
        rawSize <- B8.hGetSome h 4

        if B8.length rawSize /= 4
            then return Nothing
            else do
                let bytes = B8.append (B8.replicate 4 '\0') rawSize
                    size  = decode (B.fromStrict bytes) :: Int
                result <- B8.hGetSome h size
                return . Just $ result


protocol :: ByteString -> ByteString
protocol message =
        B8.append (len message) message

    where len = B8.drop 4 . B.toStrict . encode . B8.length


client :: Context -> [String] -> IO (Maybe String)
client Nothing _ = return Nothing

client (Just c) query = do
        protoSend c . B8.pack . intercalate "\n" $ query
        result <- protoRead c
        case result of
            Nothing -> return Nothing
            Just s  -> return . Just . B8.unpack $ s

jClient :: Context -> [String] -> IO (Maybe B.ByteString)
jClient Nothing _ = return Nothing

jClient (Just c) query = do
        protoSend c . B8.pack . intercalate "\n" $ query
        Just . B.fromStrict . fromMaybe B8.empty <$> protoRead c
