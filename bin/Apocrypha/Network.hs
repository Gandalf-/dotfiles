module Apocrypha.Network
    ( client
    , jClient
    , getContext
    , Context
    ) where

import Control.Exception (SomeException, try)
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (send, recv)

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as B

import Data.Binary (encode, decode)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)

type Context = Maybe Socket

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
protoLen b = maximum [0, decode (B.fromStrict bytes) :: Int]
  where bytes = B8.take 8 . B8.append (B8.replicate 4 '\0') $ b

_query :: Maybe Socket -> [String] -> IO B8.ByteString
_query Nothing _ = return B8.empty
_query (Just sock) msg = do
    _ <- send sock . protocol . intercalate "\n" $ msg
    size   <- recv sock 4
    let s = protoLen size
    if s > 0
      then recv sock s
      else return B8.empty

query :: Maybe Socket -> [String] -> IO (Maybe String)
query sock message = do
    buffer <- _query sock message
    return $ unprotocol buffer

-- put (Just s) msg = send s $ protocol msg
-- get (Just s) = recv s

jsonQuery :: Maybe Socket -> [String] -> IO B.ByteString
jsonQuery sock message = do
    buffer <- _query sock message
    return $ B.fromStrict buffer

type ExceptOrIO = IO (Either SomeException ())

getSocket :: Maybe String -> Maybe Integer -> IO (Maybe Socket)
getSocket host port = withSocketsDo $ do
    addrInfo <- getAddrInfo Nothing (Just h) (Just $ show p)
    let serverAddr = head addrInfo
    sock <- socket (addrFamily serverAddr) Stream defaultProtocol
    canConnect <- try (connect sock (addrAddress serverAddr)
                      ) :: ExceptOrIO
    case canConnect of
      Left  _ -> return Nothing
      Right _ -> return $ Just sock
    where h = fromMaybe "127.0.0.1" host
          p = fromMaybe 9999 port


client :: Context -> [String] -> IO (Maybe String)
client s@(Just _) message = query s message
client Nothing message = do
    -- didn't give us a socket? try to get our own
    s <- getSocket Nothing Nothing
    query s message

jClient :: Context -> [String] -> IO B.ByteString
jClient s@(Just _) message = jsonQuery s message
jClient Nothing message = do
    -- didn't give us a socket? try to get our own
    s <- getSocket Nothing Nothing
    jsonQuery s message

getContext = getSocket
