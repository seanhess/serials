{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Serials.Model.Scan where

import Data.Time
import Data.Text
import Data.Aeson
import Data.Data

import Database.RethinkDB.NoClash

import GHC.Generics

data Scan = Scan {
  date :: UTCTime,
  total :: Int,
  new :: [Text],
  updated :: [Text]
} deriving (Show, Eq, Generic)

instance ToJSON Scan
instance FromJSON Scan
instance ToDatum Scan
instance FromDatum Scan

