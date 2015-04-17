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
chaptersBySource h sid = run h $ chaptersTable # getAll sourceIndex sid

chaptersInit :: RethinkDBHandle -> IO ()
chaptersInit h = do
    initDb $ run h $ tableCreate chaptersTable
    initDb $ run h $ chaptersTable # indexCreate "sourceId" (!"sourceId")

