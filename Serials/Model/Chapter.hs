{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Serials.Model.Chapter where

import Prelude hiding (id, lookup)


import Data.Function (on)
import Data.List (sortBy)
import Data.Text (Text, unpack)
import Data.Aeson (ToJSON, FromJSON)

import Control.Applicative

import GHC.Generics
import qualified Database.RethinkDB.NoClash as R
import Database.RethinkDB.NoClash hiding (table)

import Serials.Model.Crud

-- these are the things you can change
data Chapter = Chapter {
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

sourceIndex = Index "sourceId"

bySource :: RethinkDBHandle -> Text -> IO [ChapterSettings]
bySource h id = sortByNum <$> (run h $ table # getAll sourceIndex [expr id])

sortByNum :: [ChapterSettings] -> [ChapterSettings]
sortByNum cs = sortBy (compare `on` (number . current)) cs

toChapter :: Functor f => IO (f ChapterSettings) -> IO (f Chapter)
toChapter action = do
    cs <- action
    return $ fmap current cs

find :: RethinkDBHandle -> Text -> IO (Maybe ChapterSettings)
find h id = run h $ table # get (expr id)

save :: RethinkDBHandle -> ChapterSettings -> IO (Either RethinkDBError ())
save h c = run h $ table # get (expr $ id c) # replace (const $ toDatum c)

saveEdits :: RethinkDBHandle -> Text -> Chapter -> IO (Either RethinkDBError ())
saveEdits h id c = run h $ table # get (expr $ id) # update (\row -> merge row ["edits" := toDatum c])

saveAll :: RethinkDBHandle -> [ChapterSettings] -> IO [Either RethinkDBError ()]
saveAll h cs = mapM (save h) cs

init :: RethinkDBHandle -> IO ()
init h = do
    initDb $ run h $ tableCreate table
    initDb $ run h $ table # indexCreate "sourceId" (!"sourceId")

chapterId :: Chapter -> Text
chapterId c = url c
