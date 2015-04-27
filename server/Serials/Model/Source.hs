{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Serials.Model.Source where

import Prelude hiding (id)

import Control.Applicative

import Data.Text (Text, unpack)
import Data.Aeson (ToJSON, FromJSON)
import Data.Pool

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

list :: Pool RethinkDBHandle -> IO [Source]
list h = runPool h $ table

find :: Pool RethinkDBHandle -> Text -> IO (Maybe Source)
find h id = runPool h $ table # get (expr id)

insert :: Pool RethinkDBHandle -> Source -> IO Text
insert h s = do
    r <- runPool h $ table # create s
    return $ generatedKey r

save :: Pool RethinkDBHandle -> Text -> Source -> IO ()
save h id s = runPool h $ table # get (expr id) # replace (const (toDatum s))

remove :: Pool RethinkDBHandle -> Text -> IO ()
remove h id = runPool h $ table # get (expr id) # delete

init :: Pool RethinkDBHandle -> IO ()
init h = do
    initDb $ runPool h $ tableCreate table
