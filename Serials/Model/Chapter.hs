{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Serials.Model.Chapter where

import Prelude hiding (id)

import Control.Applicative

import Data.Text (Text, unpack)
import Data.Aeson (ToJSON, FromJSON)

import GHC.Generics
import Database.RethinkDB.NoClash

import Serials.Model.Crud


data Chapter = Chapter {
  id :: Maybe Text,

  sourceId :: Text,

  chapterNumber :: Int,
  chapterName :: Text,
  chapterURL :: Text,
  chapterHidden :: Bool

} deriving (Show, Generic)

instance FromJSON Chapter
instance ToJSON Chapter
instance FromDatum Chapter
instance ToDatum Chapter

chaptersTable= table "chapters"

sourceIndex = Index "sourceId"

chaptersBySource :: RethinkDBHandle -> Text -> IO [Chapter]
chaptersBySource h sid = run h $ chaptersTable # getAll sourceIndex [expr sid] # orderBy [asc "chapterNumber"]

chapterSave :: RethinkDBHandle -> Chapter -> IO (Either RethinkDBError Datum)
chapterSave h c = run h $ chaptersTable # get (expr $ id c') # replace (const $ toDatum c')
  where
    c' = c { id = Just (chapterURL c) }

chaptersSave :: RethinkDBHandle -> [Chapter] -> IO [Either RethinkDBError Datum]
chaptersSave h cs = mapM (chapterSave h) cs

chaptersInit :: RethinkDBHandle -> IO ()
chaptersInit h = do
    initDb $ run h $ tableCreate chaptersTable
    initDb $ run h $ chaptersTable # indexCreate "sourceId" (!"sourceId")


--chapterId :: Chapter -> Maybe Text
--chapterId c = do
  --uri <- parseURIReference $ unpack $ chapterURL c
  --return $ pack (uriPath uri <> uriQuery uri)

