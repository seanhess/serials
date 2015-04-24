{-# LANGUAGE DeriveGeneric #-}

module Serials.Model.App where

import GHC.Generics
import Data.Aeson
import Data.Text

data AppInfo = AppInfo {
  appName :: Text,
  appVersion :: Text
} deriving (Eq, Generic)

instance FromJSON AppInfo
instance ToJSON AppInfo

