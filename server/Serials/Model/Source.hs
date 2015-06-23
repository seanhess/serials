{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Serials.Model.Source where

import Prelude hiding (id)

import Control.Applicative

import Data.Text (Text, unpack)
import Data.Aeson (ToJSON(..), FromJSON)
import Data.Pool
import Data.Time

import GHC.Generics
import qualified Database.RethinkDB.NoClash as R
import Database.RethinkDB.NoClash hiding (table, status, toJSON, Change)

import Serials.Model.Lib.Crud
import Serials.Model.Scan
import Serials.Link.Import (ImportSettings)
import Serials.Model.User (SecureUser(..))

instance FromJSON ImportSettings
instance ToJSON ImportSettings

data Status = Complete | Active | Disabled | Abandoned | Proposed deriving (Show, Eq, Generic)
instance FromJSON Status
instance ToJSON Status

data Source = Source {
  id :: Text,
  url :: Text,
  name :: Text,
  author :: Text,
  authorUrl :: Text,
  hidden :: Bool,

  status :: Status,

  imageUrl :: Text,
  imageMissingTitle :: Bool,

  imageArtist :: Maybe Text,
  imageArtistUrl :: Maybe Text,
  imageArtistAboutUrl :: Maybe Text,

  importSettings :: ImportSettings,

  lastScan :: Maybe Scan

} deriving (Show, Generic)
instance FromJSON Source
instance ToJSON Source
instance FromDatum Source
instance ToDatum Source

data ChangeKind = Edit | Create deriving (Show, Eq, Generic)
instance FromJSON ChangeKind
instance ToJSON ChangeKind

data Change = Change {
  source :: Source,
  kind :: ChangeKind,
  createdAt :: UTCTime,
  createdBy :: SecureUser
} deriving (Show, Generic)
instance FromJSON Change
instance ToJSON Change
instance FromDatum Change
instance ToDatum Change

table = R.table "sources"
tableChanges = R.table "sources_changes"

list :: Pool RethinkDBHandle -> IO [Source]
list h = runPool h $ table # orderBy [asc "id"]

find :: Pool RethinkDBHandle -> Text -> IO (Maybe Source)
find h id = runPool h $ table # get (expr id)

insert :: Pool RethinkDBHandle -> Source -> IO Text
insert h s = do
    r <- runPool h $ table   # create s
    return $ generatedKey r

save :: Pool RethinkDBHandle -> Text -> Source -> IO ()
save h id s = do
    runPool h $ table # get (expr id) # replace (const (toDatum s))

updateLastScan :: Pool RethinkDBHandle -> Text -> Scan -> IO (Either RethinkDBError WriteResponse)
updateLastScan h id s = runPool h $ table # get (expr id) # update (const ["lastScan" := (toDatum s)])

clearLastScan :: Pool RethinkDBHandle -> Text -> IO ()
clearLastScan h id = runPool h $ table # get (expr id) # update (const ["lastScan" := Null])

init :: Pool RethinkDBHandle -> IO ()
init h = do
    initDb $ runPool h $ tableCreate table
    initDb $ runPool h $ tableCreate tableChanges

isActive :: Source -> Bool
isActive = (== Active) . status

--------------------------------------------------------------

saveChange :: Pool RethinkDBHandle -> Change -> IO ()
saveChange h c = runPool h $ tableChanges # create c

change :: ChangeKind -> SecureUser -> Source -> IO Change
change kind user source = do
    time <- getCurrentTime
    return $ Change source kind time user
