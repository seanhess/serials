{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Serials.Model.Subscription where

import Prelude hiding (id)

import Data.Text (Text, unpack)
import Data.Aeson (ToJSON, FromJSON)
import Data.Time
import Data.Pool
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)

import Debug.Trace

import Database.RethinkDB.NoClash hiding (table)
import qualified Database.RethinkDB as R

import Control.Applicative

import GHC.Generics

import Serials.Model.Lib.Crud
import qualified Serials.Model.Source as Source
import Serials.Model.Source (Source(..))
import qualified Serials.Model.User as User
import Serials.Model.User (User(..), SecureUser(..))

data Subscription = Subscription {
  id :: Text,
  userId :: Text,
  sourceId :: Text,
  added :: UTCTime,
  subscribed :: Bool,
  chapters :: HashMap Text SubChapter
} deriving (Show, Generic)

data SubChapter = SubChapter {
  chapterId :: Text,
  read :: Bool
} deriving (Show, Generic)

instance FromJSON Subscription
instance ToJSON Subscription
instance FromDatum Subscription
instance ToDatum Subscription

instance FromJSON SubChapter
instance ToJSON SubChapter

table = R.table "subscriptions"

userField = "userId"
userIndex = Index userField

sourceField = "sourceId" :: Text
sourceIndex = Index sourceField

booksByUser :: Pool RethinkDBHandle -> Text -> IO [Source]
booksByUser h id = runPool h $ table
  # getAll userIndex [expr id]
  # eqJoin sourceField (Source.table) (Index "id")
  # R.zip # orderBy [asc "id"]

subsByUser :: Pool RethinkDBHandle -> Text -> IO [Subscription]
subsByUser h id = runPool h $ table # getAll userIndex [expr id] # orderBy [asc "id"]

usersSubscribed :: Pool RethinkDBHandle -> Text -> IO [User]
usersSubscribed h sourceId = runPool h $ table
  # getAll sourceIndex [expr sourceId]
  # eqJoin userField (User.table) (Index "id")
  # R.zip # orderBy [asc "id"]

add :: Pool RethinkDBHandle -> Text -> Text -> IO ()
add h uid sid = do
    time <- getCurrentTime
    msub <- find h uid sid
    let id  = subId uid sid
        def = Subscription id uid sid time True HashMap.empty
        sub = fromMaybe def msub
        sub' = sub { subscribed = True }

    save h uid sid sub'

save :: Pool RethinkDBHandle -> Text -> Text -> Subscription -> IO ()
save h uid sid sub = runPool h $ table # get (expr (subId uid sid)) # replace (const $ toDatum sub)

find :: Pool RethinkDBHandle -> Text -> Text -> IO (Maybe Subscription)
find h uid sid = runPool h $ table # get (expr (subId uid sid))

remove :: Pool RethinkDBHandle -> Text -> Text -> IO ()
remove h uid sid = runPool h $ table # get (expr (subId uid sid)) # update (const ["subscribed" := False])

subId :: Text -> Text -> Text
subId userId sourceId = userId <> "-" <> sourceId

init :: Pool RethinkDBHandle -> IO ()
init h = do
    initDb $ runPool h $ tableCreate table
    initDb $ runPool h $ table # indexCreate (userField) (!expr userField)
    initDb $ runPool h $ table # indexCreate (sourceField) (!expr sourceField)
