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
import Database.RethinkDB.NoClash hiding (table, status, toJSON)

import Serials.Model.Lib.Crud
import Serials.Model.Scan
import Serials.Link.Import (ImportSettings)

data Status = Complete | Active | Disabled | Abandoned | Hidden deriving (Show, Eq, Generic)

data Source = Source {
  id :: Text,
  url :: Text,
  name :: Text,
  author :: Text,

  status :: Status,

  imageUrl :: Text,
  imageMissingTitle :: Bool,

  importSettings :: ImportSettings,

  lastScan :: Maybe Scan

} deriving (Show, Generic)

instance FromJSON Status
instance ToJSON Status

instance FromJSON ImportSettings
instance ToJSON ImportSettings

instance FromJSON Source
instance ToJSON Source

instance FromDatum Source
instance ToDatum Source

table = R.table "sources"

list :: Pool RethinkDBHandle -> IO [Source]
list h = runPool h $ table # orderBy [asc "id"]

find :: Pool RethinkDBHandle -> Text -> IO (Maybe Source)
find h id = runPool h $ table # get (expr id)

insert :: Pool RethinkDBHandle -> Source -> IO Text
insert h s = do
    r <- runPool h $ table # create s
    return $ generatedKey r

save :: Pool RethinkDBHandle -> Text -> Source -> IO ()
save h id s = runPool h $ table # get (expr id) # replace (const (toDatum s))

remove :: Pool RethinkDBHandle -> Text -> IO ()
remove h id = runPool h $ table # get (expr id) # delete

updateLastScan :: Pool RethinkDBHandle -> Text -> Scan -> IO (Either RethinkDBError WriteResponse)
updateLastScan h id s = runPool h $ table # get (expr id) # update (const ["lastScan" := (toDatum s)])

clearLastScan :: Pool RethinkDBHandle -> Text -> IO ()
clearLastScan h id = runPool h $ table # get (expr id) # update (const ["lastScan" := Null])

init :: Pool RethinkDBHandle -> IO ()
init h = do
    initDb $ runPool h $ tableCreate table

isActive :: Source -> Bool
isActive = (== Active) . status
