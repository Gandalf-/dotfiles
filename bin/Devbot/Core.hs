{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Devbot.Core where

import GHC.Generics
import Apocrypha.Client

import Data.Aeson

import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import Control.Monad (liftM3)
import Text.Read (readMaybe)
import qualified Data.Text as T


data Event = Event Name Config Data
    deriving (Show, Eq)
type Name = String

data Data = Data
          { duration :: Integer
          , when     :: Integer
          , errors   :: Maybe Integer
          }
    deriving ( Show
             , Eq
             , Generic)

instance FromJSON Data where

instance ToJSON Data where
    toEncoding = genericToEncoding defaultOptions

data Config = Config
            { action   :: [String]
            , interval :: Integer
            , require  :: Maybe String
            }
    deriving ( Eq
             , Show
             , Generic)

instance FromJSON Config where
    parseJSON = withObject "Config" $ \o -> do
        -- action may be a string or list of string
        action   <- asum [
                do a <- o .: "action"
                   case a of
                       (Array _)             -> parseJSON a
                       (Data.Aeson.String s) -> return [T.unpack s]
                       _                     -> fail ""
            ]

        -- interval may be a string or number
        interval <- asum [
                do i <- o .: "interval"
                   case i of
                       (Number _)            -> parseJSON i
                       (Data.Aeson.String s) -> return . parse . T.unpack $ s
                       _                     -> fail ""
            ]

        require  <- o .:? "require"

        return Config{..}
        where
              parse "hourly" = hour
              parse "daily"  = daily
              parse "weekly" = weekly
              parse n        = fromMaybe daily (readMaybe n :: Maybe Integer)

              weekly = daily * 7
              daily  = hour * 24
              hour   = minute * 60
              minute = 60


instance ToJSON Config where
    toEncoding = genericToEncoding defaultOptions

devbot :: Context -> [String] -> IO (Maybe String)
devbot context items =
    get context $ "devbot" : items


getEvent :: Context -> String -> IO (Maybe Event)
getEvent context name = do
    -- it's fine for Data to be missing, use a default when it is

    c <- getter context ["devbot", "events", name] :: IO (Maybe Config)
    d <- getter context ["devbot", "data"  , name] :: IO (Maybe Data)

    return $ liftM3 Event
        (Just name)
        c
        (case d of
           Nothing  -> defaultData
           (Just _) -> d)

    where defaultData = Just $ Data 0 0 Nothing

events :: IO [Maybe Event]
events = do
    context <- getContext Nothing Nothing -- (Just "aspen.anardil.net") Nothing
    keys context ["devbot", "events"] >>= mapM (getEvent context)
