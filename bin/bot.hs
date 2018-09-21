module Main where

import Apocrypha.Client
import Devbot.Core

import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Concurrent (threadDelay)
import Data.List (intercalate)
import System.Process ( spawnCommand
                      , getProcessExitCode
                      , ProcessHandle
                      , waitForProcess
                      )
import System.Exit (ExitCode(..))
import System.IO (hSetBuffering, stdout, BufferMode(..))


type State = [Task]
data Task = Task Event (Maybe ProcessHandle) StartTime
type StartTime = Integer


startingState :: [Maybe Event] -> State
startingState es =
    map (\e -> Task e Nothing 0) $ convert es
    where
        convert :: [Maybe a] -> [a]
        convert [] = []
        convert (Nothing:xs) = convert xs
        convert (Just e :xs) = e : convert xs


runner :: State -> IO State
runner state = do
    -- check each Task for something to do

    threadDelay $ 1 * second
    mapM handle state >>= runner

    where second = 1000000


handle :: Task -> IO Task
handle task@(Task (Event _ _ d) Nothing _) = do
    -- not currently running
    time <- getTime

    if ready time d
        then check task
        else return task
    where
        ready :: Integer -> Data -> Bool
        ready now (Data _ when _) = now > when

handle task@(Task _ (Just h) _) = do
    code <- getProcessExitCode h
    case code of
        -- still running
        Nothing          -> return task

        -- finished
        Just ExitSuccess -> success task
        Just _           -> failure task

check :: Task -> IO Task
check task@(Task (Event n c _) _ _) = do
    met <- requirementsMet n c
    if met
        then run task
        else return task

run :: Task -> IO Task
run (Task event@(Event n (Config c _ _) _) _ _) = do
    -- start running the actions, add handle to Task
    putStrLn $ "Running: " ++ n ++ ":\n" ++ cmd

    h <- spawnCommand cmd
    Task event (Just h) <$> getTime

    where cmd = intercalate "\n" . map ("  " ++) $ c


success :: Task -> IO Task
success (Task event@(Event _ c _) _ startTime) = do
    -- the command succeeded, set errors to Nothing, and determine next
    -- time to run
    duration <- negate . (startTime -) <$> getTime

    let newEvent = clearErrors $ updateTime event next duration
        newTask  = Task newEvent Nothing 0

    _ <- flush newEvent
    return newTask

    where next = nextRun startTime c

          clearErrors :: Event -> Event
          clearErrors (Event n co (Data du wh _)) =
            Event n co (Data du wh Nothing)

failure :: Task -> IO Task
failure (Task event@(Event n _ d) _ startTime) = do
    -- the command failed, log the error, increment errors and set next
    -- time to retry
    logger $ concat ["running ", n
                    , " failed, backing off "
                    , show backoff, " seconds"]

    -- next <- getTime >>= (return . (+ 1))
    next <- (+ backoff) <$> getTime
    duration <- negate . (startTime -) <$> getTime

    let newEvent = incrementError $ updateTime event next duration
        newTask  = Task newEvent Nothing 0

    _ <- flush newEvent
    return newTask

    where backoff = getBackoff d

          getBackoff :: Data -> Integer
          getBackoff (Data _ _ Nothing)  = 10
          getBackoff (Data _ _ (Just e)) = (e + 1) * 10

          incrementError :: Event -> Event
          incrementError (Event n c (Data du wh Nothing)) =
              Event n c (Data du wh (Just 1))

          incrementError (Event n c (Data du wh (Just e))) =
              Event n c (Data du wh (Just $ e + 1))

updateTime :: Event -> Integer -> Integer -> Event
updateTime (Event n c (Data _ _ e)) newTime duration =
  Event n c (Data duration newTime e)

nextRun :: Integer -> Config -> Integer
nextRun time (Config _ interval _) =
        time + interval

flush :: Event -> IO Event
flush e@(Event n _ (Data duration when errors)) = do
    c <- getContext Nothing Nothing

    set c ["devbot", "data", n, "duration"] duration
    set c ["devbot", "data", n, "when"    ] when

    case errors of
        Nothing -> del c ["devbot", "data", n, "errors"]
        Just v  -> set c ["devbot", "data", n, "errors"] v

    return e

logger :: String -> IO ()
logger msg = do
    time <- getTime
    putStrLn $ "devbot: " ++ show time ++ " " ++ msg


requirementsMet :: String -> Config -> IO Bool
requirementsMet _ (Config _ _ Nothing) = return True
requirementsMet n (Config _ _ (Just r)) = do
    req <- get' ["devbot", "requirements", r]

    case req of
        Nothing -> do
            logger doesntExist
            return False
        (Just a) -> runCheck a
    where
          runCheck :: String -> IO Bool
          runCheck cmd = do
              h <- spawnCommand cmd
              code <- waitForProcess h
              case code of
                  ExitSuccess -> return True
                  _           -> do
                      logger cmdFailed
                      return False

          doesntExist =
            n ++ " references requirement that doesn't exist"

          cmdFailed =
            "requirement " ++ r ++ " for " ++ n ++ " not met"


getTime :: IO Integer
getTime = round `fmap` getPOSIXTime


main :: IO ()
main = do
    putStrLn "devbot starting up"
    hSetBuffering stdout LineBuffering
    es <- events
    _ <- runner $ startingState es
    return ()
