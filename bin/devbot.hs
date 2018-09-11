import Control.Exception (SomeException, try)
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (send, recv)

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as B

import Data.Binary (encode)

import Data.List (intercalate)
import Data.Time.Clock.POSIX (getPOSIXTime)

import Control.Monad (liftM2, liftM3, liftM4)
import Text.Read (readMaybe)


data Event = Event Config Data
    deriving (Show, Eq)

data Data = Data Duration When Errors
    deriving (Show, Eq)
type Duration = Integer
type When = Integer
type Errors = Maybe Integer

data Config = Config Name Action Interval Require
    deriving (Eq, Show)
type Name = String
type Action = [String]
type Interval = String
type Require = Maybe String

devbot :: Context -> [String] -> IO (Maybe String)
devbot context items =
    get context $ "devbot" : items

maybeInt :: Maybe String -> Maybe Integer
maybeInt = maybe Nothing (\y -> readMaybe y :: Maybe Integer)

getData :: Context -> String -> IO (Maybe Data)
getData context event = do
    let c = context

    d <- devbot c ["data", event, "duration"]
    w <- devbot c ["data", event, "when"]
    r <- devbot c ["data", event, "errors"]

    let duration = maybeInt d
        when     = maybeInt w
        errors   = Just (maybeInt r)

    return $ liftM3 Data duration when errors

getConfig :: Context -> String -> IO (Maybe Config)
getConfig context event = do
    let c = context

    a <- devbot c ["events", event, "action"]
    i <- devbot c ["events", event, "interval"]
    r <- devbot c ["events", event, "require"]

    let name     = Just event
        action   = fmap lines a
        interval = i
        require  = Just r

    return $ liftM4 Config name action interval require

getEvent :: Context -> String -> IO (Maybe Event)
getEvent context event = do
    c <- getConfig context event
    d <- getData context event

    return $ liftM2 Event c d

events :: IO [Maybe Event]
events = do
    context <- getContext
    keys context ["devbot", "events"] >>= mapM (getEvent context)

now :: IO Integer
now = round `fmap` getPOSIXTime

printAction :: Config -> String
printAction (Config _ action _ _ )  = decorate a blue
    where a = "    " ++ intercalate pad action
          blue = (Blue, Black, Null) :: Decoration
          pad = "\n    "

printName :: Config -> String
printName (Config name _ _ _) = decorate name green
    where green = (Green, Black, Bold) :: Decoration

prettyTime :: Integer -> String
prettyTime i
    | i <= minute = show i ++ " seconds"
    | i <= hour   = show (div i minute) ++ " minutes"
    | i <= day    = show (div i hour) ++ " hours"
    | otherwise   = show (div i day) ++ " days"
    where day = 86400
          hour = 3600
          minute = 60

secondsOrTime :: String -> Maybe Integer -> String
secondsOrTime d Nothing   = d
secondsOrTime _ (Just i) = "every " ++ s
    where s = prettyTime i

printInterval :: Config -> String
printInterval (Config _ _ interval _) = decorate ("    " ++ i) cyan
    where i = secondsOrTime interval (readMaybe interval :: Maybe Integer)
          cyan = (Cyan, Black, Null) :: Decoration

printNext :: Data -> Integer -> String
printNext (Data _ when _) time = decorate ("next in " ++ n) yellow
    where n = prettyTime $ when - time
          yellow = (Yellow, Black, Null)


printOptional :: Config -> Data -> IO ()
printOptional (Config _ _ _ require) (Data _ _ errors) = do
    printErrors errors
    printRequire require
    putStrLn ""
    where
          printErrors :: Maybe Integer -> IO ()
          printErrors Nothing = return ()
          printErrors (Just s) =
                putStr $ ", " ++ decorate (show s ++ " errors") red

          printRequire :: Maybe String -> IO ()
          printRequire Nothing = return ()
          printRequire (Just r') =
              putStr $ ", requires " ++ r'

          red = (Red, Black, Null) :: Decoration

printEvent :: Maybe Event -> IO ()
printEvent Nothing = return ()
printEvent (Just (Event c d)) = do
    putStrLn $ printName c
    putStrLn $ printAction c
    putStr $ printInterval c
    time <- now
    putStr $ ", " ++ printNext d time
    printOptional c d
    putStrLn ""

main :: IO ()
main = events >>= mapM_ printEvent

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


keys :: Context -> [String] -> IO [String]
keys con items = do
    result <- client con $ items ++ ["--keys"]
    return $ maybe [] words result

get :: Context -> [String] -> IO (Maybe String)
get = client

set :: Context -> [String] -> String -> IO (Maybe String)
set con items value =
    client con $ items ++ ["=", value]


-- create a new socket every time versions
keys' :: [String] -> IO [String]
keys' items = do
    result <- client Nothing $ items ++ ["--keys"]
    return $ maybe [] words result

get' :: [String] -> IO (Maybe String)
get' = client Nothing

set' :: [String] -> String -> IO (Maybe String)
set' items value = client Nothing $ items ++ ["=", value]

-- https://gist.github.com/nakagami8/3952274

data Color = NoColor | Black | Red | Green | Yellow
           | Blue | Magenta | Cyan  | White
           deriving (Show,Ord,Eq)

data Attribute = Null | Bold | Underscore | Blink | Reverse | Concealed
                 deriving (Show,Ord,Eq)

type FgColor = Color
type BgColor = Color
type Decoration = (FgColor, BgColor, Attribute)

decorate :: String -> Decoration -> String
decorate str dec = concat [escDec dec, str, esc 0]

esc :: Int -> String
esc n = concat ["\ESC[", show n, "m"]

escDec :: Decoration -> String
escDec (fg, bg, at) = concat [escFg fg, escBg bg, escAttribute at]

escFg :: FgColor -> String
escFg color = escColor color 0

escBg :: BgColor -> String
escBg color = escColor color 10

escColor :: Color -> Int -> String
escColor NoColor _ = ""
escColor Black   offset = esc (30 + offset)
escColor Red     offset = esc (31 + offset)
escColor Green   offset = esc (32 + offset)
escColor Yellow  offset = esc (33 + offset)
escColor Blue    offset = esc (34 + offset)
escColor Magenta offset = esc (35 + offset)
escColor Cyan    offset = esc (36 + offset)
escColor White   offset = esc (37 + offset)

escAttribute :: Attribute -> String
escAttribute Null       = ""
escAttribute Bold       = esc 1
escAttribute Underscore = esc 4
escAttribute Blink      = esc 5
escAttribute Reverse    = esc 7
escAttribute Concealed  = esc 8
