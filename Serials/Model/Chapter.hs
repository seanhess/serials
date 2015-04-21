{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Serials.Model.Chapter where

import Prelude hiding (id, lookup, dropWhile, filter, drop)

import Data.Char (isAlphaNum)
import Data.Function (on)
import Data.List (sortBy)
import Data.Text (Text, unpack, dropWhile, filter, drop)
import Data.Aeson (ToJSON, FromJSON)

import Control.Applicative

import GHC.Generics
import qualified Database.RethinkDB.NoClash as R
import Database.RethinkDB.NoClash hiding (table)

import Safe (headMay)
import Serials.Model.Crud

import Numeric

-- these are the things you can change
-- they need an id if you're going to send them down :(
data Chapter = Chapter {
  chapterId :: Maybe Text,
  number :: Int,
  name :: Text,
  url :: Text
} deriving (Show, Generic)

data ChapterSettings = ChapterSettings {
  id :: Text,
  sourceId :: Text,
  edits :: Maybe Chapter,
  hidden :: Bool,
  current :: Chapter
} deriving (Show, Generic)

instance FromJSON ChapterSettings
instance ToJSON ChapterSettings
instance FromDatum ChapterSettings
instance ToDatum ChapterSettings

instance FromJSON Chapter
instance ToJSON Chapter
instance FromDatum Chapter
instance ToDatum Chapter

table = R.table "chapters"

sourceIndexName = "sourceId"
sourceIndex = Index sourceIndexName

urlIndexName = "current.url"
urlIndex = Index urlIndexName

bySource :: RethinkDBHandle -> Text -> IO [ChapterSettings]
bySource h id = sortByNum <$> (run h $ table # getAll sourceIndex [expr id])

findByURL :: RethinkDBHandle -> Text -> IO (Maybe ChapterSettings)
findByURL h url = headMay <$> (run h $ byURL url :: IO [ChapterSettings])

byURL :: Text -> ReQL
byURL url = table # getAll urlIndex [expr url] 

sortByNum :: [ChapterSettings] -> [ChapterSettings]
sortByNum cs = sortBy (compare `on` (number . current)) cs

toChapter :: Functor f => IO (f ChapterSettings) -> IO (f Chapter)
toChapter action = do
    cs <- action
    return $ fmap (withId . current) cs

find :: RethinkDBHandle -> Text -> IO (Maybe ChapterSettings)
find h id = run h $ table # get (expr id)

save :: RethinkDBHandle -> ChapterSettings -> IO (Either RethinkDBError Datum)
save h c = run h $ table # get (expr (id c)) # replace (const $ toDatum c)

saveEdits :: RethinkDBHandle -> Text -> Chapter -> IO (Either RethinkDBError Datum)
saveEdits h id c = run h $ table # get (expr $ id) # update (const ["edits" := toDatum c, "current" := toDatum c])

saveAll :: RethinkDBHandle -> [ChapterSettings] -> IO [Either RethinkDBError Datum]
saveAll h cs = mapM (save h) cs

init :: RethinkDBHandle -> IO ()
init h = do
    initDb $ run h $ tableCreate table
    initDb $ run h $ table # indexCreate (unpack sourceIndexName) (!expr sourceIndexName)
    initDb $ run h $ table # indexCreate (unpack urlIndexName) (\row -> expr (row ! "current" ! "url"))

urlId :: Chapter -> Text
urlId c = filter isAlphaNum $ drop 5 $ (url c)

currentURL :: ChapterSettings -> Text
currentURL = url . current

withId :: Chapter -> Chapter
withId c = c { chapterId = Just $ urlId c }

