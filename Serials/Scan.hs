{-# LANGUAGE OverloadedStrings #-}

module Serials.Scan where

import Prelude hiding (id)

import Data.Text (Text)
import Database.RethinkDB.NoClash

import Serials.Model.Crud
import Serials.Link
import Serials.Model.Source (Source, sourceUrl, sourcesFind, importSettings)
import qualified Serials.Model.Source as Source
import Serials.Model.Chapter

scanSource :: Source -> IO [Link]
scanSource s = links (sourceUrl s) (importSettings s)

linkToChapter :: Source -> Link -> Chapter
linkToChapter s (Link url text) = Chapter {
  id = Nothing,
  sourceId = Source.id s,
  chapterName = text,
  chapterURL = url,
  chapterHidden = False
}

importSource :: RethinkDBHandle -> Text -> IO ()
importSource h sourceId = do
  Just s <- sourcesFind h sourceId
  ls <- scanSource s
  let cs = map (linkToChapter s) ls
  chaptersSave h cs


