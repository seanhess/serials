{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Serials.Model.App where

import GHC.Generics
import Data.Aeson
import Data.Pool
import Data.Text

import Database.RethinkDB.NoClash
import Serials.Model.Lib.Crud

data AppInfo = AppInfo {
  appName :: Text,
  appVersion :: Text
} deriving (Eq, Generic)

instance FromJSON AppInfo
instance ToJSON AppInfo

data AppStatus = AppStatus {
  health :: Text
} deriving (Generic)
instance ToJSON AppStatus

appStatus :: Pool RethinkDBHandle -> IO AppStatus
appStatus h = do
  runPool h $ table "sources" # status :: IO (Either RethinkDBError Datum)
  return $ AppStatus "OK"



