{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Serials.Model.Source where

import Prelude hiding (id)

import Control.Applicative

import Data.Text (Text, unpack)
import Data.Aeson (ToJSON(..), FromJSON, Value(..), toJSON)
import Data.Pool
import Data.Time
import qualified Data.HashMap.Strict as HashMap

import GHC.Generics
import qualified Database.RethinkDB.NoClash as R
import Database.RethinkDB.NoClash hiding (table, status, toJSON, Change, Object, Null)

import Serials.Model.Lib.Crud
import Serials.Model.Chapter (Chapter(..))
import Serials.Model.Scan (Scan(..))
import Serials.Link.Import (ImportSettings)

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

  changeId :: Maybe Text,

  status :: Status,

  imageUrl :: Text,
  imageMissingTitle :: Bool,

  imageArtist :: Maybe Text,
  imageArtistUrl :: Maybe Text,
  imageArtistAboutUrl :: Maybe Text,

  importSettings :: ImportSettings,

  lastScan :: Maybe Scan,

  chapters :: [Chapter]

} deriving (Show, Generic, Eq)

instance FromJSON Source
instance ToJSON Source
instance FromDatum Source
instance ToDatum Source

-- when they are displayed in a list
newtype SourceThumbnail = SourceThumbnail Source deriving (Show, Generic)

instance FromJSON SourceThumbnail
instance ToJSON SourceThumbnail where
  toJSON (SourceThumbnail source) = Object $ foldr HashMap.delete obj ["chapters", "lastScan", "importSettings"]
    where (Object obj) = toJSON source

table = R.table "sources"

list :: Pool RethinkDBHandle -> IO [Source]
list h = runPool h $ table # orderBy [asc "id"]

find :: Pool RethinkDBHandle -> Text -> IO (Maybe Source)
find h id = runPool h $ table # get (expr id)


insert :: Pool RethinkDBHandle -> Source -> IO Text
insert h s = do
    r <- runPool h $ table   # create s
    return $ generatedKey r

save :: Pool RethinkDBHandle -> Text -> Source -> IO (Either RethinkDBError ())
save h id s = do
    er <- runPool h $ table # get (expr id) # replace (const (toDatum s)) :: IO (Either RethinkDBError WriteResponse)
    return $ fmap (return ()) er

delete :: Pool RethinkDBHandle -> Text -> IO ()
delete = docsRemove table

init :: Pool RethinkDBHandle -> IO ()
init h = do
    initDb $ runPool h $ tableCreate table

isActive :: Source -> Bool
isActive = (== Active) . status

deleteChapters :: Pool RethinkDBHandle -> Text -> IO ()
deleteChapters h sourceId = runPool h $ table
  # get (expr sourceId)
  # update (const ["chapters" := (toDatum empty)])

  where
  empty :: [Chapter]
  empty = []


------------------------------------------------------------------------------------

updateLastScan :: Pool RethinkDBHandle -> Text -> Scan -> IO (Either RethinkDBError WriteResponse)
updateLastScan h sourceId s = runPool h $ table # get (expr sourceId) # update (const ["lastScan" := (toDatum s)])

clearLastScan :: Pool RethinkDBHandle -> Text -> IO ()
clearLastScan h sourceId = runPool h $ table # get (expr sourceId) # update (const ["lastScan" := Null])
