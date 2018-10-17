{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Devbot.Core where

import           Apocrypha.Client
import           GHC.Generics

import           Data.Aeson

import           Data.Foldable       (asum)
import           Data.Maybe          (fromMaybe)
import           Text.Read           (readMaybe)

import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T


type ConfigMap = HM.HashMap String Config
type DataMap   = HM.HashMap String Data


data Event = Event Name Config Data
    deriving (Show, Eq)
type Name = String


data Data = Data
          { duration :: Integer
          , when     :: Integer
          , errors   :: Maybe Integer
          }
    deriving (Show, Eq, Generic)

instance FromJSON Data where

instance ToJSON Data where
    toEncoding = genericToEncoding defaultOptions


data Config = Config
            { action   :: [String]
            , interval :: Integer
            , require  :: Maybe String
            }
    deriving (Eq, Show, Generic)

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
              parse :: String -> Integer
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


events :: IO [Event]
events = do
    c <- getContext Nothing
    cs <- get c ["devbot", "events"] :: IO (Maybe ConfigMap)
    ds <- get c ["devbot", "data"  ] :: IO (Maybe DataMap)

    let configs = HM.toList . fromMaybe (HM.fromList []) $ cs
        datas = fromMaybe (HM.fromList []) ds

        parse :: (String, Config) -> Event
        parse (name, config) =
            Event name config . fromMaybe defaultData . HM.lookup name $ datas

    return $ map parse configs
    where
          defaultData = Data 0 0 Nothing
