{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Serials.Model.Chapter where

import Prelude hiding (id, lookup)

import Control.Applicative

import Data.Text (Text, unpack)
import Data.Aeson (ToJSON, FromJSON)

import GHC.Generics
import qualified Database.RethinkDB.NoClash as R
import Database.RethinkDB.NoClash hiding (table)

import Serials.Model.Crud


data Chapter = Chapter {
  id :: Text,

  sourceId :: Text,

  number :: Int,
  name :: Text,
  url :: Text,
  hidden :: Bool

} deriving (Show, Generic)

instance FromJSON Chapter
instance ToJSON Chapter
instance FromDatum Chapter
instance ToDatum Chapter

table = R.table "chapters"
editTable = R.table "chapters_edit"

sourceIndex = Index "sourceId"

bySource :: RethinkDBHandle -> Text -> IO [Chapter]
bySource = findBySource table

editsBySource :: RethinkDBHandle -> Text -> IO [Chapter]
editsBySource = findBySource editTable

findBySource t h sid = run h $ t # getAll sourceIndex [expr sid] # orderBy [asc "number"]

find :: RethinkDBHandle -> Text -> IO (Maybe Chapter)
find h id = run h $ table # get (expr id)

saveTo :: Table -> RethinkDBHandle -> Chapter -> IO (Either RethinkDBError ())
saveTo t h c = run h $ t # get (expr $ id c) # replace (const $ toDatum c)

saveEdit :: RethinkDBHandle -> Chapter -> IO (Either RethinkDBError ())
saveEdit = saveTo editTable

saveScanned :: RethinkDBHandle -> Chapter -> IO (Either RethinkDBError ())
saveScanned = saveTo table

saveAllScanned :: RethinkDBHandle -> [Chapter] -> IO [Either RethinkDBError ()]
saveAllScanned h cs = mapM (saveScanned h) cs

init :: RethinkDBHandle -> IO ()
init h = do
    initDb $ run h $ tableCreate table
    initDb $ run h $ table # indexCreate "sourceId" (!"sourceId")

    initDb $ run h $ tableCreate editTable
    initDb $ run h $ editTable # indexCreate "sourceId" (!"sourceId")


--chapterId :: Chapter -> Maybe Text
--chapterId c = do
  --uri <- parseURIReference $ unpack $ chapterURL c
  --return $ pack (uriPath uri <> uriQuery uri)

