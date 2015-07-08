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
import Serials.AppMonad

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

-- could I pass in both the table and the database connection?
-- sure... why not?
-- then I could do the magic sauce... 
-- runr $ table # orderBy [asc "id"]
-- well, just start with the database connection

list :: App [Source]
list = docsList table

find :: Text -> App (Maybe Source)
find = docsFind table

findByTag :: Tag -> App [Source]
findByTag tag = runDb $ table # getAll tagIndex [expr tag]

-- either way this is slow right? So just get all the tags and tally it yourself
allTags :: App [TagCount]
allTags = do
    tss <- runDb $ table # R.map (!"tags") :: App [[Tag]]
    let grouped = group $ sort $ concat tss
        counts  = map (\ts -> TagCount (head ts) (length ts)) grouped
    return $ sortBy (\a b -> compare (tagCount b) (tagCount a)) counts

insert :: Source -> App Text
insert = docsInsert table

save :: Text -> Source -> App (Either RethinkDBError ())
save id s = do
    er <- runDb $ table # get (expr id) # replace (const (toDatum s)) :: App (Either RethinkDBError WriteResponse)
    return $ fmap (return ()) er

delete :: Text -> App ()
delete = docsRemove table

init :: App ()
init = do
    initDb $ runDb $ tableCreate table
    initDb $ runDb $ table # ex indexCreate ["multi" := True] (tagIndexName) (!expr tagIndexName)

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

updateLastScan :: Text -> Scan -> App (Either RethinkDBError WriteResponse)
updateLastScan sourceId s = runDb $ table # get (expr sourceId) # update (const ["lastScan" := (toDatum s)])

clearLastScan :: Text -> App ()
clearLastScan sourceId = runDb $ table # get (expr sourceId) # update (const ["lastScan" := Null])
