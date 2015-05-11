{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Serials.Model.Chapter where

import Prelude hiding (id, lookup, dropWhile, filter, drop)

import Data.Char (isAlphaNum)
import Data.Function (on)
import Data.List (sortBy)
import Data.Text (Text, unpack, dropWhile, filter, drop)
import Data.Aeson (ToJSON, FromJSON)
import Data.Pool
import Data.Time
import Data.Either

import Control.Applicative

import GHC.Generics
import qualified Database.RethinkDB.NoClash as R
import Database.RethinkDB.NoClash hiding (table)

import Safe (headMay)
import Serials.Model.Lib.Crud
import Serials.Link.Link

import Numeric

-- these are the things you can change
-- they need an id if you're going to send them down :(
data Chapter = Chapter {
  id :: Text,
  sourceId :: Text,
  added :: UTCTime,
  number :: Int,
  hidden :: Bool,
  edited :: Bool,
  content :: Content
} deriving (Show, Generic, Eq)

instance FromJSON Chapter
instance ToJSON Chapter
instance FromDatum Chapter
instance ToDatum Chapter

table = R.table "chapters"

sourceIndexName = "sourceId"
sourceIndex = Index sourceIndexName

--urlIndexName = "url"
--urlIndex = Index urlIndexName

bySource :: Pool RethinkDBHandle -> Text -> IO [Chapter]
bySource h id = runPool h $ table # getAll sourceIndex [expr id] # orderBy [asc "number"]

deleteBySource :: Pool RethinkDBHandle -> Text -> IO ()
deleteBySource h id = runPool h $ table # getAll sourceIndex [expr id] # delete

--findByURL :: Pool RethinkDBHandle -> Text -> IO (Maybe Chapter)
--findByURL h url = headMay <$> (runPool h $ byURL url :: IO [Chapter])

--byURL :: Text -> ReQL
--byURL url = table # getAll urlIndex [expr url] 

find :: Pool RethinkDBHandle -> Text -> IO (Maybe Chapter)
find h id = runPool h $ table # get (expr id)

save :: Pool RethinkDBHandle -> Chapter -> IO (Either RethinkDBError ())
save h c = do
  res <- runPool h $ table # get (expr (id c)) # replace (const $ toDatum c) :: IO (Either RethinkDBError Datum)
  case res of 
    Left err -> return $ Left err
    Right d  -> return $ Right ()

saveAll :: Pool RethinkDBHandle -> [Chapter] -> IO (Either [RethinkDBError] ())
saveAll h cs = do
    res <- mapM (save h) cs
    return $ case lefts res of
      []   -> Right ()
      errs -> Left errs

init :: Pool RethinkDBHandle -> IO ()
init h = do
    initDb $ runPool h $ tableCreate table
    initDb $ runPool h $ table # indexCreate (unpack sourceIndexName) (!expr sourceIndexName)
    --initDb $ runPool h $ table # indexCreate (unpack urlIndexName) (\row -> expr (row ! "url"))

urlId :: Text -> Text
urlId u = filter isAlphaNum $ drop 5 $ u

remove :: Pool RethinkDBHandle -> Text -> IO ()
remove h id = runPool h $ table # get (expr id) # delete

--withId :: Chapter -> Chapter
--withId c = c { chapterId = Just $ urlId c }

