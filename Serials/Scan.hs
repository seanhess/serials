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
import Serials.Model.Chapter (Chapter(..))
import Serials.Model.Source (Source(..))

import Data.HashMap.Strict (HashMap, fromList, lookup)

scanSource :: Source -> IO [Link]
scanSource s = links (Source.url s) (importSettings s)

linkToChapter :: Text -> (Int, Link) -> Chapter
linkToChapter sid (n, (Link url text)) = Chapter {
  Chapter.id       = Chapter.urlId url,
  Chapter.sourceId = sid,
  Chapter.number = n,
  Chapter.name = text,
  Chapter.url = url,
  Chapter.edited    = False,
  Chapter.hidden   = False,
  Chapter.link = Link url text
}

importSource :: RethinkDBHandle -> Text -> IO (Either [RethinkDBError] ())
importSource h sourceId = do
  putStrLn $ "Scanning: " <> show sourceId
  Just source <- Source.find h sourceId
  links <- scanSource source
  let scannedChapters = map (linkToChapter (Source.id source)) (zip [1..] links)

  -- ok I've got a bunch of chapters
  -- now I need to merge them with the current ones
  edits <- chapterMap <$> Chapter.bySource h sourceId

  let mergedChapters = mergeAll (Source.id source) edits scannedChapters

  --print $ take 1 mergedChapters

  -- now I need to save all of them :)
  res <- Chapter.saveAll h mergedChapters
  return $ case lefts res of
    [] -> Right ()
    errs -> Left errs

-- Merging ---------------------------------------------------

mergeChapter :: Text -> Maybe Chapter -> Chapter -> Chapter
mergeChapter sourceId Nothing c = c
mergeChapter sourceId (Just old) c
  | Chapter.edited old = old
  | otherwise          = c

mergeAll :: Text -> HashMap Text Chapter -> [Chapter] -> [Chapter]
mergeAll sourceId cm cs = map merge cs
  where 
    merge c = mergeChapter sourceId (lookup (Chapter.id c) cm) c

chapterMap :: [Chapter] -> HashMap Text Chapter
chapterMap = fromList . map (\c -> (Chapter.id c, c))


