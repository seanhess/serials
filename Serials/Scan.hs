{-# LANGUAGE OverloadedStrings #-}

module Serials.Scan where

import Prelude hiding (id, lookup)

import Data.Text (Text)
import Database.RethinkDB.NoClash
import Data.Either (lefts)

import Data.Monoid ((<>))

import Serials.Model.Crud
import Serials.Link
import qualified Serials.Model.Source as Source
import qualified Serials.Model.Chapter as Chapter
import Serials.Model.Chapter (Chapter(..))
import Serials.Model.Source (Source(..))

import Data.HashMap.Strict (HashMap, fromList, lookup)

scanSource :: Source -> IO [Link]
scanSource s = links (Source.url s) (importSettings s)

linkToChapter :: Source -> (Int, Link) -> Chapter
linkToChapter s (n, (Link url text)) = Chapter {
  Chapter.id = url,
  Chapter.sourceId = Source.id s,
  Chapter.number = n,
  Chapter.name = text,
  Chapter.url = url,
  Chapter.hidden = False
}

importSource :: RethinkDBHandle -> Text -> IO (Either [RethinkDBError] ())
importSource h sourceId = do
  Just source <- Source.find h sourceId
  links <- scanSource source
  let scannedChapters = map (linkToChapter source) (zip [1..] links)

  edits <- Chapter.editsBySource h sourceId

  let mergedChapters = mergeWithEdits (chapterMap edits) scannedChapters

  res <- Chapter.saveAllScanned h mergedChapters
  return $ case lefts res of
    [] -> Right ()
    errs -> Left errs

-- Merging ---------------------------------------------------

mergeWithEdit :: Maybe Chapter -> Chapter -> Chapter
mergeWithEdit Nothing c = c
mergeWithEdit (Just edit) c = edit

mergeWithEdits :: HashMap Text Chapter -> [Chapter] -> [Chapter]
mergeWithEdits cm cs = map merge cs
  where 
    merge c = mergeWithEdit (lookup (Chapter.id c) cm) c

chapterMap :: [Chapter] -> HashMap Text Chapter
chapterMap = fromList . map (\c -> (Chapter.id c, c))


