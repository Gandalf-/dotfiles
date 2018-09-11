module Apocrypha.Network (client, getContext, Context) where

import Control.Exception (SomeException, try)
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (send, recv)

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as B

import Data.Binary (encode)
import Data.List (intercalate)

type Context = Maybe Socket

protocol :: String -> B8.ByteString
protocol message = 
    B8.append len msg
    where len = htonl' $ length message
          msg = B8.pack message
          htonl' = B8.drop 4 . B.toStrict . encode

unprotocol :: B8.ByteString -> Maybe String
unprotocol bytes = clean result
  where result = B8.unpack $ B8.drop 4 bytes
        clean [] = Nothing
        clean xs = Just $ init xs

query :: Socket -> [String] -> IO (Maybe String)
query sock message = do
    _ <- send sock $ protocol msg
    buffer <- recv sock bufferSize
    return $ unprotocol buffer
    where msg = intercalate "\n" message
          bufferSize = 1024 ^ 2

type ExceptOrIO = IO (Either SomeException ())

getSocket :: IO (Maybe Socket)
getSocket = withSocketsDo $ do
    addrInfo <- getAddrInfo Nothing (Just host) (Just $ show port)
    let serverAddr = head addrInfo
    sock <- socket (addrFamily serverAddr) Stream defaultProtocol
    canConnect <- try (connect sock (addrAddress serverAddr)
                      ) :: ExceptOrIO
    case canConnect of
      Left  _ -> return Nothing
      Right _ -> return $ Just sock
    where host = "127.0.0.1"
          port = 9999


client :: Context -> [String] -> IO (Maybe String)
client sock message = 

    case sock of
      Nothing -> do
        -- didn't give us a socket? try to get our own
        s <- getSocket
        case s of
          Nothing -> return Nothing
          Just s' -> query s' message

      -- use their socket
      Just s  -> query s message


getContext = getSocket
