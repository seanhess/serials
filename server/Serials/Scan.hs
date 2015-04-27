{-# LANGUAGE OverloadedStrings #-}

module Serials.Scan where

import Prelude hiding (id, lookup)

import Data.Text (Text)
import Database.RethinkDB.NoClash
import Data.Either (lefts)
import Data.Pool
import Data.Monoid ((<>))
import Data.Time

import Control.Applicative

import Serials.Model.Crud
import Serials.Link
import qualified Serials.Model.Source as Source
import qualified Serials.Model.Chapter as Chapter
import Serials.Model.Chapter (Chapter(..))
import Serials.Model.Source (Source(..))

import Data.HashMap.Strict (HashMap, fromList, lookup)

data MergeResult = New | Updated | Edited | Same deriving (Show, Eq)

scanSource :: Source -> IO [Link]
scanSource s = links (Source.url s) (importSettings s)

linkToChapter :: Text -> UTCTime -> Link -> Chapter
linkToChapter sid time (Link n url text) = Chapter {
  Chapter.id       = Chapter.urlId url,
  Chapter.sourceId = sid,
  Chapter.scanned = time,
  Chapter.number = n,
  Chapter.name = text,
  Chapter.url = url,
  Chapter.edited    = False,
  Chapter.hidden   = False,
  Chapter.link = Link n url text
}

importSource :: Pool RethinkDBHandle -> Text -> IO (Either [RethinkDBError] ())
importSource h sourceId = do
  putStrLn $ "Scanning: " <> show sourceId
  Just source <- Source.find h sourceId
  links <- scanSource source
  time <- getCurrentTime
  let scannedChapters = map (linkToChapter (Source.id source) time) links

  -- ok I've got a bunch of chapters
  -- now I need to merge them with the current ones
  edits <- chapterMap <$> Chapter.bySource h sourceId

  let merged = mergeAll edits scannedChapters
      new = map snd $ filter (isMergeType New) merged
      ups = map snd $ filter (isMergeType Updated) merged

  putStrLn $ " - NEW " <> show new
  putStrLn $ " - UPDATED " <> show ups

  -- I need to save all the new ones and all the updated ones
  resNew <- Chapter.saveAll h new
  resUps <- Chapter.saveAll h ups

  let errs = lefts resNew <> lefts resUps

  return $ case errs of
    [] -> Right ()
    errs -> Left errs

-- Merging ---------------------------------------------------

mergeChapter :: Maybe Chapter -> Chapter -> (MergeResult, Chapter)
mergeChapter Nothing c = (New, c)
mergeChapter (Just old) c
  | Chapter.edited old                  = (Edited,  old)
  | Chapter.link old /= Chapter.link c  = (Updated, c)
  | otherwise                           = (Same,    old)

mergeAll :: HashMap Text Chapter -> [Chapter] -> [(MergeResult, Chapter)]
mergeAll cm cs = map merge cs
  where 
    merge c = mergeChapter (lookup (Chapter.id c) cm) c

chapterMap :: [Chapter] -> HashMap Text Chapter
chapterMap = fromList . map (\c -> (Chapter.id c, c))

isMergeType :: MergeResult -> (MergeResult, Chapter) -> Bool
isMergeType r = (== r) . fst


