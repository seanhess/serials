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

data ChangeKind = Edit | Create deriving (Show, Eq, Generic)
instance FromJSON ChangeKind
instance ToJSON ChangeKind

data Change = Change {
  id :: Text,
  oldSource :: Source,
  newSource :: Source,
  kind :: ChangeKind,
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

findById :: Pool RethinkDBHandle -> Text -> IO (Maybe Change)
findById = docsFind table

save :: Pool RethinkDBHandle -> Change -> IO ()
save h c = runPool h $ table # create c

change :: ChangeKind -> SecureUser -> Source -> IO Change
change kind user source = do
    time <- getCurrentTime
    return $ Change "" source kind time user

findBySourceId :: Pool RethinkDBHandle -> Text -> IO [Change]
findBySourceId h id = runPool h $ table # getAll sourceIndex [expr id] # orderBy [desc "createdAt"]




