{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Serials.Model.Scan where

import Prelude hiding (id)

import Control.Applicative

import Data.Text (Text, unpack)
import Data.Aeson (ToJSON, FromJSON)

import GHC.Generics
import Database.RethinkDB.NoClash

import Serials.Model.Crud
import Serials.Link (Link)

-- What am I doing?
-- press a button, initiate a scan
-- store the chapters
-- show the chapters

-- you can store them all as one big thing
-- should I store every version of them? (NO)

-- should I store them on the source?
-- sure! Then I can use that one admin page for everything...

--data Scan = Scan {
  --id :: Maybe Text,
  --scanDate :: Text,
  --scanLinks :: [Link],
  --scanComplete :: Bool
--} deriving (Show, Generic)

--instance FromJSON Scan
--instance ToJSON Scan
--instance FromDatum Scan
--instance ToDatum Scan

--scansTable = table "scans"

--scansBySource :: RethinkDBHandle -> IO [Scan]
--scansBySource h = run h $ scansTable

