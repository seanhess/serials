{-# LANGUAGE OverloadedStrings #-}

module Serials.Scan where

import Prelude hiding (id, lookup)

import Data.Text (Text)
import Database.RethinkDB.NoClash
import Data.Either (lefts)

import Data.Monoid ((<>))

import Control.Applicative

import Serials.Model.Crud
import Serials.Link
import qualified Serials.Model.Source as Source
import qualified Serials.Model.Chapter as Chapter
import Serials.Model.Chapter (Chapter(..), ChapterSettings(..))
import Serials.Model.Source (Source(..))

import Data.HashMap.Strict (HashMap, fromList, lookup)

scanSource :: Source -> IO [Link]
scanSource s = links (Source.url s) (importSettings s)

linkToChapter :: (Int, Link) -> Chapter
linkToChapter (n, (Link url text)) = Chapter {
  Chapter.number = n,
  Chapter.name = text,
  Chapter.url = url
}

chapterSettings :: Text -> Chapter -> ChapterSettings
chapterSettings sid c = ChapterSettings {
  Chapter.id       = (Chapter.chapterId c),
  Chapter.sourceId = sid,
  Chapter.edits    = Nothing,
  Chapter.hidden   = False,
  Chapter.current  = c
}

importSource :: RethinkDBHandle -> Text -> IO (Either [RethinkDBError] ())
importSource h sourceId = do
  Just source <- Source.find h sourceId
  links <- scanSource source
  let scannedChapters = map (linkToChapter) (zip [1..] links)

  -- ok I've got a bunch of chapters
  -- now I need to merge them with the current ones
  edits <- chapterMap <$> Chapter.bySource h sourceId

  let mergedChapters = mergeAll (Source.id source) edits scannedChapters

  -- now I need to save all of them :)
  res <- Chapter.saveAll h mergedChapters
  return $ case lefts res of
    [] -> Right ()
    errs -> Left errs

-- Merging ---------------------------------------------------

mergeChapter :: Text -> Maybe ChapterSettings -> Chapter -> ChapterSettings
mergeChapter sourceId Nothing c = chapterSettings sourceId c
mergeChapter sourceId (Just old) c = 
  case Chapter.edits old of
    Nothing    -> old { Chapter.current = c }
    Just edits -> old

mergeAll :: Text -> HashMap Text ChapterSettings -> [Chapter] -> [ChapterSettings]
mergeAll sourceId cm cs = map merge cs
  where 
    merge c = mergeChapter sourceId (lookup (Chapter.chapterId c) cm) c

chapterMap :: [ChapterSettings] -> HashMap Text ChapterSettings
chapterMap = fromList . map (\c -> (Chapter.id c, c))


