{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Serials.Model.Change where

import Data.Text (Text, unpack)
import Data.Aeson (ToJSON(..), FromJSON)
import Data.Pool
import Data.Time

import Database.RethinkDB.NoClash hiding (table, status, toJSON, Change)
import qualified Database.RethinkDB.NoClash as R

import GHC.Generics

import Serials.Model.Source (Source(..))
import Serials.Model.User (SecureUser(..))
import Serials.Model.Lib.Crud

data Change = Change {
  id :: Text,
  baseId :: Maybe Text,
  source :: Source,
  createdAt :: UTCTime,
  createdBy :: SecureUser
} deriving (Show, Generic)
instance FromJSON Change
instance ToJSON Change
instance FromDatum Change
instance ToDatum Change

table = R.table "sources_changes"

sourceIndexName = "source.id"
sourceIndex = Index sourceIndexName

init :: Pool RethinkDBHandle -> IO ()
init h = do
    initDb $ runPool h $ tableCreate table
    initDb $ runPool h $ table # indexCreate (unpack sourceIndexName) (\row -> expr (row ! "source" ! "id"))

list :: Pool RethinkDBHandle -> IO [Change]
list h = runPool h $ table # orderBy [desc "createdAt"]

findById :: Pool RethinkDBHandle -> Text -> IO (Maybe Change)
findById = docsFind table

insert :: Pool RethinkDBHandle -> Change -> IO Text
insert = docsInsert table

change :: Maybe Text -> Source -> UTCTime -> SecureUser -> Change
change = Change ""

findBySourceId :: Pool RethinkDBHandle -> Text -> IO [Change]
findBySourceId h id = runPool h $ table # getAll sourceIndex [expr id] # orderBy [desc "createdAt"]




