{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Serials.Model.Source where

import Prelude hiding (id)

import Control.Applicative

import Data.Text (Text, unpack)
import Data.Aeson (ToJSON, FromJSON)

import GHC.Generics
import qualified Database.RethinkDB.NoClash as R
import Database.RethinkDB.NoClash hiding (table)

import Serials.Model.Crud
import Serials.Link.Import (ImportSettings)

data Source = Source {
  id :: Text,
  url :: Text,
  name :: Text,
  disabled :: Maybe Bool,

  imageUrl :: Text,

  importSettings :: ImportSettings
} deriving (Show, Generic)


instance FromJSON ImportSettings
instance ToJSON ImportSettings

instance FromJSON Source
instance ToJSON Source
instance FromDatum Source
instance ToDatum Source

table = R.table "sources"

list :: RethinkDBHandle -> IO [Source]
list h = run h $ table

find :: RethinkDBHandle -> Text -> IO (Maybe Source)
find h id = run h $ table # get (expr id)

insert :: RethinkDBHandle -> Source -> IO Text
insert h s = do
    r <- run h $ table # create s
    return $ generatedKey r

save :: RethinkDBHandle -> Text -> Source -> IO ()
save h id s = run h $ table # get (expr id) # replace (const (toDatum s))

remove :: RethinkDBHandle -> Text -> IO ()
remove h id = run h $ table # get (expr id) # delete

init :: RethinkDBHandle -> IO ()
init h = do
    initDb $ run h $ tableCreate table
