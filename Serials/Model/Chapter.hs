{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Serials.Model.Chapter where

import Prelude hiding (id, lookup, dropWhile, filter, drop)

import Data.Char (isAlphaNum)
import Data.Function (on)
import Data.List (sortBy)
import Data.Text (Text, unpack, dropWhile, filter, drop)
import Data.Aeson (ToJSON, FromJSON)

import Control.Applicative

import GHC.Generics
import qualified Database.RethinkDB.NoClash as R
import Database.RethinkDB.NoClash hiding (table)

import Safe (headMay)
import Serials.Model.Crud
import Serials.Link.Link

import Numeric

-- these are the things you can change
-- they need an id if you're going to send them down :(
data Chapter = Chapter {
  id :: Text,
  sourceId :: Text,

  number :: Int,
  name :: Text,
  url :: Text,

  hidden :: Bool,
  edited :: Bool,
  link :: Link
} deriving (Show, Generic)

instance FromJSON Chapter
instance ToJSON Chapter
instance FromDatum Chapter
instance ToDatum Chapter

table = R.table "chapters"

sourceIndexName = "sourceId"
sourceIndex = Index sourceIndexName

--urlIndexName = "url"
--urlIndex = Index urlIndexName

bySource :: RethinkDBHandle -> Text -> IO [Chapter]
bySource h id = run h $ table # getAll sourceIndex [expr id] # orderBy [asc "number"]

deleteBySource :: RethinkDBHandle -> Text -> IO ()
deleteBySource h id = run h $ table # getAll sourceIndex [expr id] # delete

--findByURL :: RethinkDBHandle -> Text -> IO (Maybe Chapter)
--findByURL h url = headMay <$> (run h $ byURL url :: IO [Chapter])

--byURL :: Text -> ReQL
--byURL url = table # getAll urlIndex [expr url] 

find :: RethinkDBHandle -> Text -> IO (Maybe Chapter)
find h id = run h $ table # get (expr id)

save :: RethinkDBHandle -> Chapter -> IO (Either RethinkDBError ())
save h c = do
  res <- run h $ table # get (expr (id c)) # replace (const $ toDatum c) :: IO (Either RethinkDBError Datum)
  case res of 
    Left err -> return $ Left err
    Right d  -> return $ Right ()

saveAll :: RethinkDBHandle -> [Chapter] -> IO [Either RethinkDBError ()]
saveAll h cs = mapM (save h) cs

urlId :: Text -> Text
urlId u = filter isAlphaNum $ drop 5 $ u

--withId :: Chapter -> Chapter
--withId c = c { chapterId = Just $ urlId c }

