{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Serials.Model.Subscription where

import Prelude hiding (id)

import Control.Monad.IO.Class (liftIO)

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

import Serials.Types

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

booksByUser :: Text -> App [Source]
booksByUser id = runDb $ table
  # getAll userIndex [expr id]
  # eqJoin sourceField (Source.table) (Index "id")
  # R.zip # orderBy [asc "id"]

subsByUser :: Text -> App [Subscription]
subsByUser id = runDb $ table # getAll userIndex [expr id] # orderBy [asc "id"]

usersSubscribed :: Text -> App [User]
usersSubscribed sourceId = runDb $ table
  # getAll sourceIndex [expr sourceId]
  # eqJoin userField (User.table) (Index "id")
  # R.zip # orderBy [asc "id"]

add :: Text -> Text -> App ()
add uid sid = do
    time <- liftIO $ getCurrentTime
    msub <- find uid sid
    let id  = subId uid sid
        def = Subscription id uid sid time True HashMap.empty
        sub = fromMaybe def msub
        sub' = sub { subscribed = True }

    save uid sid sub'

save :: Text -> Text -> Subscription -> App ()
save uid sid sub = runDb $ table # get (expr (subId uid sid)) # replace (const $ toDatum sub)

find :: Text -> Text -> App (Maybe Subscription)
find uid sid = runDb $ table # get (expr (subId uid sid))

remove :: Text -> Text -> App ()
remove uid sid = runDb $ table # get (expr (subId uid sid)) # update (const ["subscribed" := False])

subId :: Text -> Text -> Text
subId userId sourceId = userId <> "-" <> sourceId

init :: App ()
init = do
    initDb $ runDb $ tableCreate table
    initDb $ runDb $ table # indexCreate (userField) (!expr userField)
    initDb $ runDb $ table # indexCreate (sourceField) (!expr sourceField)
