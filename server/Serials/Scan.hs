{-# LANGUAGE OverloadedStrings #-}

module Serials.Scan where

import Prelude hiding (id, lookup)

import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.RethinkDB.NoClash
import Data.Either (lefts)
import Data.Pool
import Data.Monoid ((<>))
import Data.Time
import Data.List (nubBy)
import Debug.Trace

import Control.Applicative
import Control.Concurrent.PooledIO.Independent
import Control.Concurrent
import Control.Monad
import Control.Exception

import Serials.Model.Lib.Crud
import Serials.Link
import qualified Serials.Model.Source as Source
import qualified Serials.Model.Chapter as Chapter
import qualified Serials.Model.Scan as Scan
import Serials.Model.Chapter (Chapter(..))
import Serials.Model.Source (Source(..), Status(..))
import Serials.Model.Scan (Scan(..))

import Serials.Notify

import System.IO

import Data.HashMap.Strict (HashMap, fromList, lookup)

data MergeResult = New | Updated | Edited | Same deriving (Show, Eq)

data ScanResult = ScanResult {
  allChapters :: [Chapter],
  newChapters :: [Chapter],
  updatedChapters :: [Chapter]
} deriving (Eq, Show)

scanSourceContent :: Source -> IO [Content]
scanSourceContent s = importContent (Source.url s) (importSettings s)

-- this only turns a Link into a chapter, not a Title
linkToChapter :: Text -> UTCTime -> Int -> Content -> Chapter
linkToChapter sid time n content =
  Chapter {
    Chapter.id       = makeId content,
    Chapter.added = time,
    Chapter.edited    = False,
    Chapter.hidden   = False,
    Chapter.content = content
  }
  where
  makeId (Link url _) = Chapter.urlId url
  makeId (Title text) = sid <> Text.filter isAlphaNum text

importSourceId :: Pool RethinkDBHandle -> Text -> IO ()
importSourceId h sourceId = do
    Just source <- Source.find h sourceId
    importSource h source

scanSourceChapters :: Source -> IO [Chapter]
scanSourceChapters source = do
  content <- scanSourceContent source
  time <- getCurrentTime
  return $ allChapters $ scanResult source time content

skipSource :: Source -> IO ()
skipSource source = do
  putStrLn $ " skip  " <> scanShowSource source

scanResult :: Source -> UTCTime -> [Content] -> ScanResult
scanResult source time content = ScanResult all new ups
  where
  scannedChapters = map (uncurry $ linkToChapter sid time) (zip [10,20..] content)
  edits = chapterMap $ Source.chapters source
  merged = mergeAll edits $ nubBy idEqual scannedChapters
  new = map snd $ filter (isMergeType New) merged
  ups = map snd $ filter (isMergeType Updated) merged
  all = map snd $ merged

  sid = Source.id source
  idEqual a b = Chapter.id a == Chapter.id b

importSource :: Pool RethinkDBHandle -> Source -> IO ()
importSource h source = do

  -- update last scan saying it started
  Source.clearLastScan h sid

  content <- scanSourceContent source
  time <- getCurrentTime

  let ScanResult all new ups = scanResult source time content
      scan = Scan time (length all) (map Chapter.id new) (map Chapter.id ups)

  let source' = source { chapters = all, lastScan = Just scan }

  mapM (log " updated ") ups
  mapM (log "     new ") new

  print source'

  -- notify all
  -- skip this step if all the chapters are new.
  -- or if the book is inactive
  when (length new < length all && Source.status source == Active) $ do
    notifyChapters h source new

  -- this means it actually completed, so go last?
  checkErr $ Source.save h sid source'

  where
    sid = Source.id source
    idEqual a b = Chapter.id a == Chapter.id b
    log msg c = putStrLn $ msg <> (show $ Source.name source) <> " " <> show c

scanShowSource :: Source -> String
scanShowSource s = show (Source.id s) <> " " <> show (Source.status s) <> " " <> show (Source.name s)

importAllSources :: Pool RethinkDBHandle -> IO ()
importAllSources h = do
    time <- getCurrentTime
    putStr $ show time <> " | "
    hSetBuffering stdout LineBuffering
    sources <- Source.list h
    let active = filter Source.isActive sources
        inactive = filter (not . Source.isActive) sources
    --mapM_ (skipSource) inactive
    putStr $ (show $ length active) <> "/" <> (show $ length sources) <> " sources"
    putStrLn ""

    -- importSource can return some information
    mapM_ (importSource h) active

    -- for now, don't do it in parallel
    --runException (Just 5) $ map (importSource h) sources

-- Merging ---------------------------------------------------

mergeChapter :: Maybe Chapter -> Chapter -> (MergeResult, Chapter)
mergeChapter Nothing c = (New, c)
mergeChapter (Just old) c
  | Chapter.edited old                  = (Edited,  old)
  | Chapter.content old /= Chapter.content c  = (Updated, c)
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
