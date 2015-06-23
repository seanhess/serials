{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Serials.Model.Submission where

import Data.Aeson
import Data.Text (Text)
import Data.Pool
import Data.Time

import Database.RethinkDB.NoClash hiding (table, status, toJSON, Change)
import qualified Database.RethinkDB as R

import GHC.Generics

import Serials.Model.Lib.Crud
import Serials.Model.Source (Source(..))
import Serials.Model.User (User(..), SecureUser(..))

data Status = Active | Rejected | Accepted deriving (Show, Generic)
instance ToJSON Status
instance FromJSON Status

data Change = Edit | Create deriving (Show, Generic)
instance ToJSON Change
instance FromJSON Change

data Submission = Submission {
  id :: Text,
  change :: Change,
  status :: Status,
  createdBy :: SecureUser,
  createdAt :: UTCTime,
  updatedAt :: UTCTime,
  source :: Source
} deriving (Show, Generic)

instance ToJSON Submission
instance FromJSON Submission

instance FromDatum Submission
instance ToDatum Submission

table = R.table "submissions"

list :: Pool RethinkDBHandle -> IO [Submission]
list = docsList table

find :: Pool RethinkDBHandle -> Text -> IO (Maybe Submission)
find = docsFind table

insert :: Pool RethinkDBHandle -> Submission -> IO Text
insert = docsInsert table

save :: Pool RethinkDBHandle -> Text -> Submission -> IO ()
save = docsSave table

remove :: Pool RethinkDBHandle -> Text -> IO ()
remove = docsRemove table

init :: Pool RethinkDBHandle -> IO ()
init h = do
    initDb $ runPool h $ tableCreate table
