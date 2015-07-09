{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Serials.Scan where

import Prelude hiding (id, lookup)

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception

import Data.Aeson (ToJSON)
import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.RethinkDB.NoClash
import Data.Either (lefts)
import Data.Pool
import Data.Monoid ((<>))
import Data.Maybe (isNothing)
import Data.Time
import Data.List (nubBy)
import Debug.Trace

import GHC.Generics

import Serials.Model.Lib.Crud
import Serials.Link
import qualified Serials.Model.Source as Source
import qualified Serials.Model.Chapter as Chapter
import qualified Serials.Model.Scan as Scan
import Serials.Model.Chapter (Chapter(..))
import Serials.Model.Source (Source(..), Status(..))
import Serials.Model.Scan (Scan(..))

import Serials.Notify
import Serials.Types

import System.IO

import Data.HashMap.Strict (HashMap, fromList, lookup)

data MergeResult = New | Updated | Edited | Removed | Same deriving (Show, Eq)

data ScanResult = ScanResult {
  scan :: Scan,
  allChapters :: [Chapter],
  newChapters :: [Chapter],
  updatedChapters :: [Chapter]
} deriving (Eq, Show, Generic)
instance ToJSON ScanResult

scanSourceContent :: Source -> IO [Content]
scanSourceContent s = importContent (Source.url s) (importSettings s)

contentsChapters :: Text -> UTCTime -> [Content] -> [Chapter]
contentsChapters sid time content = nubBy idEqual $ map (uncurry $ linkToChapter sid time) (zip [10,20..] content)
  where idEqual a b = Chapter.id a == Chapter.id b

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

importSourceId :: Text -> App ()
importSourceId sourceId = do
    Just source <- Source.find sourceId
    importSource source

scanSourceResult :: Source -> IO ScanResult
scanSourceResult source = do
  content <- scanSourceContent source
  time <- getCurrentTime
  return $ scanResult source time content

skipSource :: Source -> IO ()
skipSource source = do
  putStrLn $ " skip  " <> scanShowSource source

scanResult :: Source -> UTCTime -> [Content] -> ScanResult
scanResult source time content = ScanResult scan all new ups
  where
  merged = mergeAll (Source.chapters source) (contentsChapters sid time content)
  new = map snd $ filter (isMergeType New) merged
  ups = map snd $ filter (isMergeType Updated) merged
  all = map snd $ merged
  scan = Scan time (length all) (map Chapter.id new) (map Chapter.id ups)

  sid = Source.id source

importSource :: Source -> App ()
importSource source = do

  -- update last scan saying it started
  Source.clearLastScan sid

  content <- liftIO $ scanSourceContent source
  time <- liftIO $ getCurrentTime

  let ScanResult scan all new ups = scanResult source time content

  let source' = source { chapters = all, lastScan = Just scan }

  liftIO $ mapM (log " updated ") ups
  liftIO $ mapM (log "     new ") new

  -- notify all
  -- skip this step if all the chapters are new.
  -- or if the book is inactive
  when (length new < length all && Source.status source == Active) $ do
    notifyChapters source new

  -- this means it actually completed, so go last?
  checkErr $ Source.save sid source'

  where
    sid = Source.id source
    idEqual a b = Chapter.id a == Chapter.id b
    log msg c = putStrLn $ msg <> (show $ Source.name source) <> " " <> show c

scanShowSource :: Source -> String
scanShowSource s = show (Source.id s) <> " " <> show (Source.status s) <> " " <> show (Source.name s)

importAllSources :: App ()
importAllSources = do
    time <- liftIO $ getCurrentTime
    liftIO $ putStr $ show time <> " | "
    liftIO $ hSetBuffering stdout LineBuffering
    sources <- Source.list
    let active = filter Source.isActive sources
        inactive = filter (not . Source.isActive) sources
    --mapM_ (skipSource) inactive
    liftIO $ putStr $ (show $ length active) <> "/" <> (show $ length sources) <> " sources"
    liftIO $ putStrLn ""

    -- importSource can return some information
    mapM_ (importSource) active

    -- for now, don't do it in parallel
    --runException (Just 5) $ map (importSource h) sources

-- Merging ---------------------------------------------------

mergeChapter :: Maybe Chapter -> Chapter -> (MergeResult, Chapter)
-- if it doesn't exist in the scan map, it's been removed
mergeChapter Nothing old = (Same, old)
mergeChapter (Just new) old
  | Chapter.edited  old                        = (Edited, old)
  | Chapter.content new /= Chapter.content old = (Updated, new)
  | otherwise                                  = (Same, old)

mergeAll :: [Chapter] -> [Chapter] -> [(MergeResult, Chapter)]
mergeAll sourceCs scanCs = current <> new
  where
    current = map merge sourceCs
    new     = map (\c -> (New, c)) $ filter isNew scanCs

    isNew c = isNothing $ lookup (Chapter.id c) sourceMap
    merge c = mergeChapter (lookup (Chapter.id c) scanMap) c
    scanMap = chapterMap scanCs
    sourceMap = chapterMap sourceCs

chapterMap :: [Chapter] -> HashMap Text Chapter
chapterMap = fromList . map (\c -> (Chapter.id c, c))

isMergeType :: MergeResult -> (MergeResult, Chapter) -> Bool
isMergeType r = (== r) . fst

---------------------------------------------------------------

checkErr :: Show a => App (Either a b) -> App ()
checkErr action = do
    res <- action
    case res of
      Left err -> liftIO $ throwIO $ (userError $ show err)
      Right _  -> return ()
    return ()
