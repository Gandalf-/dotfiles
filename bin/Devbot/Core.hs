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
import Text.Read (readMaybe)

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM


data Devbot = Devbot DataMap ConfigMap
    deriving (Show, Eq, Generic)
instance FromJSON Devbot where

type ConfigMap = HM.HashMap String Config
type DataMap   = HM.HashMap String Data


data Event = Event String Config Data
    deriving (Show, Eq)
-- type Name = String


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


{-
getEvent :: Context -> String -> IO (Maybe Event)
getEvent context name = do
    -- it's fine for Data to be missing, use a default when it is

    c <- get context ["devbot", "events", name] :: IO (Maybe Config)
    d <- get context ["devbot", "data"  , name] :: IO (Maybe Data)

    return $ result name c (fromMaybe defaultData d)

    where
          result :: String -> Maybe Config -> Data -> Maybe Event
          result _ Nothing _  = Nothing
          result s (Just c) d = Just $ Event s c d

          defaultData = Data 0 0 Nothing

events :: IO [Maybe Event]
events = do
    c <- getContext Nothing Nothing -- (Just "aspen.anardil.net") Nothing
    r <- keys c ["devbot", "events"] >>= mapM (getEvent c)
    cleanContext c
    return r
-}

events :: IO [Event]
events = do
    c <- getContext Nothing Nothing
    configs <- get c ["devbot", "events"] :: IO (Maybe ConfigMap)
    datas <- get c ["devbot", "data"] :: IO (Maybe DataMap)
    cleanContext c

    let cs = HM.toList . fromMaybe (HM.fromList []) $ configs
        ds = fromMaybe (HM.fromList []) datas

        parse :: (String, Config) -> Event
        parse (name, config) =
            Event name config (fromMaybe defaultData (HM.lookup name ds))

    return $ map parse cs
    where
          defaultData = Data 0 0 Nothing

