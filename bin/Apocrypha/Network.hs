module Apocrypha.Network
    ( client, jClient
    , Context, getContext, cleanContext
    ) where

import Control.Exception (SomeException, try)
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (send, recv)

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as B

import Data.Binary (encode, decode)
import Data.List (intercalate)


protocol :: String -> B8.ByteString
protocol message =
    B8.append len msg
    where len = htonl' $ length message
          msg = B8.pack message
          htonl' = B8.drop 4 . B.toStrict . encode


unprotocol :: B8.ByteString -> Maybe String
unprotocol bytes = clean result
  where result = B8.unpack bytes
        clean [] = Nothing
        clean xs = Just $ init xs


protoLen :: B8.ByteString -> Int
protoLen b = maximum [0, len]

  where bytes = B8.take 8 . B8.append (B8.replicate 4 '\0') $ b
        len   = decode (B.fromStrict bytes) :: Int


_query :: Maybe Socket -> [String] -> IO B8.ByteString
_query Nothing _ = return B8.empty
_query (Just sock) msg = do
    _ <- send sock . protocol . intercalate "\n" $ msg
    size <- recv sock 4
    reader sock $ protoLen size

    where
          reader :: Socket -> Int -> IO B8.ByteString
          reader sock s
            | s <= 0    = return B8.empty
            | otherwise = do
                bytes <- recv sock s
                next  <- reader sock (s - B8.length bytes)
                return $ B8.append bytes next


query :: Maybe Socket -> [String] -> IO (Maybe String)
query sock message = do
    buffer <- _query sock message
    return $ unprotocol buffer


jsonQuery :: Maybe Socket -> [String] -> IO B.ByteString
jsonQuery sock message = do
    buffer <- _query sock message
    return $ B.fromStrict buffer


type ExceptOrIO = IO (Either SomeException ())

getSocket :: Maybe (String, Integer) -> IO (Maybe Socket)
getSocket Nothing = getSocket (Just ("127.0.0.1", 9999))

getSocket (Just (host, port)) = withSocketsDo $ do
    addrInfo <- getAddrInfo Nothing (Just host) (Just $ show port)
    let serverAddr = head addrInfo
    sock <- socket (addrFamily serverAddr) Stream defaultProtocol
    canConnect <- try (connect sock (addrAddress serverAddr)
                      ) :: ExceptOrIO
    case canConnect of
      Left  _ -> return Nothing
      Right _ -> return $ Just sock


client :: Context -> [String] -> IO (Maybe String)
client s@(Just _) message = query s message
client Nothing message = do
    -- didn't give us a socket? try to get our own
    s <- getSocket Nothing
    r <- query s message
    cleanContext s
    return r


jClient :: Context -> [String] -> IO B.ByteString
jClient s@(Just _) message = jsonQuery s message
jClient Nothing message = do
    -- didn't give us a socket? try to get our own
    s <- getSocket Nothing
    r <- jsonQuery s message
    cleanContext s
    return r


type Context = Maybe Socket

getContext = getSocket

cleanContext :: Context -> IO ()
cleanContext Nothing  = return ()
cleanContext (Just s) = close s
