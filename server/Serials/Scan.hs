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
import Control.Concurrent.PooledIO.Independent
import Control.Concurrent
import Control.Monad
import Control.Exception

import Serials.Model.Crud
import Serials.Link
import qualified Serials.Model.Source as Source
import qualified Serials.Model.Chapter as Chapter
import qualified Serials.Model.Scan as Scan
import Serials.Model.Chapter (Chapter(..))
import Serials.Model.Source (Source(..))
import Serials.Model.Scan (Scan(..))

import System.IO

import Data.HashMap.Strict (HashMap, fromList, lookup)

data MergeResult = New | Updated | Edited | Same deriving (Show, Eq)

scanSource :: Source -> IO [Link]
scanSource s = links (Source.url s) (importSettings s)

linkToChapter :: Text -> UTCTime -> Link -> Chapter
linkToChapter sid time (Link n url text) = Chapter {
  Chapter.id       = Chapter.urlId url,
  Chapter.sourceId = sid,
  Chapter.added = time,
  Chapter.number = n,
  Chapter.name = text,
  Chapter.url = url,
  Chapter.edited    = False,
  Chapter.hidden   = False,
  Chapter.link = Link n url text
}

importSourceId :: Pool RethinkDBHandle -> Text -> IO ()
importSourceId h sourceId = do
    Just source <- Source.find h sourceId
    importSource h source

importSource :: Pool RethinkDBHandle -> Source -> IO ()
importSource h source = do
  putStrLn $ "SCAN " <> show sid <> " " <> name

  links <- scanSource source
  time <- getCurrentTime
  let scannedChapters = map (linkToChapter sid time) links

  edits <- chapterMap <$> Chapter.bySource h sid

  let merged = mergeAll edits scannedChapters
      new = map snd $ filter (isMergeType New) merged
      ups = map snd $ filter (isMergeType Updated) merged
      scan = Scan time (length merged) (map Chapter.id new) (map Chapter.id ups)

  putStrLn $ name
  putStrLn $ "  new     " <> show new
  putStrLn $ "  updated " <> show ups

  checkErr $ Chapter.saveAll h new
  checkErr $ Chapter.saveAll h ups

  -- this means it actually completed, so go last?
  checkErr $ Source.updateLastScan h sid scan

  putStrLn $ "DONE " <> name

  where 
    sid = Source.id source
    name = show $ Source.name source


importAllSources :: Pool RethinkDBHandle -> IO ()
importAllSources h = do
    hSetBuffering stdout LineBuffering
    sources <- Source.list h
    putStrLn $ " sources: " <> (show $ length sources)
    runException (Just 5) $ map (importSource h) sources

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

---------------------------------------------------------------

checkErr :: Show a => IO (Either a b) -> IO ()
checkErr action = do
    res <- action
    case res of
      Left err -> throwIO $ (userError $ show err)
      Right _  -> return ()
    return ()


