{-# LANGUAGE OverloadedStrings #-}

module Serials.Scan where

import Prelude hiding (id)

import Data.Text (Text)
import Database.RethinkDB.NoClash
import Data.Either (lefts)

import Data.Monoid ((<>))

import Serials.Model.Crud
import Serials.Link
import Serials.Model.Source (Source, sourceUrl, sourcesFind, importSettings)
import qualified Serials.Model.Source as Source
import Serials.Model.Chapter


scanSource :: Source -> IO [Link]
scanSource s = links (sourceUrl s) (importSettings s)

linkToChapter :: Source -> (Int, Link) -> Chapter
linkToChapter s (n, (Link url text)) = Chapter {
  id = Nothing,
  sourceId = Source.id s,
  chapterNumber = n,
  chapterName = text,
  chapterURL = url,
  chapterHidden = False
}

importSource :: RethinkDBHandle -> Text -> IO (Either [RethinkDBError] ())
importSource h sourceId = do
  Just s <- sourcesFind h sourceId
  ls <- scanSource s
  print $ length ls
  let cs = map (linkToChapter s) (zip [1..] ls)
  print $ length cs
  print $ take 1 cs
  res <- chaptersSave h cs
  return $ case lefts res of
    [] -> Right ()
    errs -> Left errs


