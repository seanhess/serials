{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Serials.Model.Source where

import Prelude hiding (id)

import Control.Applicative

import Data.Text (Text, unpack)
import Data.Aeson (ToJSON(..), FromJSON, Value(..), toJSON)
import Data.Pool
import Data.Time
import Data.List (sort, group, sortBy)
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)

import GHC.Generics
import qualified Database.RethinkDB as R
import Database.RethinkDB.NoClash hiding (table, status, toJSON, Change, Object, Null, group)

import Serials.Model.Lib.Crud
import Serials.Model.Chapter (Chapter(..))
import Serials.Model.Scan (Scan(..))
import Serials.Link.Import (ImportSettings)

instance FromJSON ImportSettings
instance ToJSON ImportSettings

data Status = Complete | Active | Disabled | Abandoned | Proposed deriving (Show, Eq, Generic)
instance FromJSON Status
instance ToJSON Status

type Tag = Text

data TagCount = TagCount {
  tagName :: Text,
  tagCount :: Int
} deriving (Eq, Show, Generic)

instance ToJSON TagCount

data Source = Source {
  id :: Text,
  url :: Text,
  name :: Text,
  author :: Text,
  authorUrl :: Text,
  hidden :: Bool,

  tags :: [Tag],

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
tagIndexName = "tags"
tagIndex     = Index tagIndexName

list :: Pool RethinkDBHandle -> IO [Source]
list h = runPool h $ table # orderBy [asc "id"]

find :: Pool RethinkDBHandle -> Text -> IO (Maybe Source)
find h id = runPool h $ table # get (expr id)

findByTag :: Pool RethinkDBHandle -> Tag -> IO [Source]
findByTag h tag = runPool h $ table # getAll tagIndex [expr tag]

-- either way this is slow right? So just get all the tags and tally it yourself
allTags :: Pool RethinkDBHandle -> IO [TagCount]
allTags h = do
    tss <- runPool h $ table # R.map (!"tags") :: IO [[Tag]]
    let grouped = group $ sort $ concat tss
        counts  = map (\ts -> TagCount (head ts) (length ts)) grouped
    return $ sortBy (\a b -> compare (tagCount b) (tagCount a)) counts

insert :: Pool RethinkDBHandle -> Source -> IO Text
insert h s = do
    r <- runPool h $ table # create s
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
    initDb $ runPool h $ table # ex indexCreate ["multi" := True] (tagIndexName) (!expr tagIndexName)

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
