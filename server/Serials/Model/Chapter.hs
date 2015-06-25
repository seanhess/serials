{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Serials.Model.Chapter where

import Prelude hiding (id, lookup, dropWhile, filter, drop)

import Data.Char (isAlphaNum)
import Data.Function (on)
import Data.List (sortBy)
import Data.Text (Text, unpack, dropWhile, filter, drop)
import Data.Aeson (ToJSON, FromJSON)
import Data.Pool
import Data.Time
import Data.Either
import Data.Maybe (fromMaybe)

import Control.Applicative

import GHC.Generics
import qualified Database.RethinkDB.NoClash as R
import Database.RethinkDB.NoClash hiding (table)

import Safe (headMay)
import Serials.Model.Lib.Crud
import Serials.Link.Link
import Serials.Model.Scan

import Numeric

data Chapter = Chapter {
  id :: Text,
  added :: UTCTime,
  edited :: Bool,
  hidden :: Bool,
  content :: Content
} deriving (Show, Generic, Eq)
instance FromJSON Chapter
instance ToJSON Chapter
instance FromDatum Chapter
instance ToDatum Chapter

urlId :: Text -> Text
urlId u = filter isAlphaNum $ drop 5 $ u

