{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Serials.Model.Source where

import Prelude hiding (id)

import Control.Applicative

import Data.Text (Text, unpack)
import Data.Aeson (ToJSON, FromJSON)

import GHC.Generics
import Database.RethinkDB.NoClash

import Serials.Model.Crud
import Serials.Link.Import (ImportSettings)

data Source = Source {
  id :: Text,
  sourceUrl :: Text,
  sourceName :: Text,
  sourceDisabled :: Maybe Bool,

  importSettings :: ImportSettings
} deriving (Show, Generic)


instance FromJSON ImportSettings
instance ToJSON ImportSettings

instance FromJSON Source
instance ToJSON Source
instance FromDatum Source
instance ToDatum Source

sourcesTable= table "sources"

sourcesList :: RethinkDBHandle -> IO [Source]
sourcesList h = run h $ sourcesTable

sourcesFind :: RethinkDBHandle -> Text -> IO (Maybe Source)
sourcesFind h id = run h $ sourcesTable # get (expr id)

sourcesCreate :: RethinkDBHandle -> Source -> IO Text
sourcesCreate h s = do
    r <- run h $ sourcesTable # create s
    return $ generatedKey r

sourcesSave :: RethinkDBHandle -> Text -> Source -> IO ()
sourcesSave h id s = run h $ sourcesTable # get (expr id) # replace (const (toDatum s))

sourcesRemove :: RethinkDBHandle -> Text -> IO ()
sourcesRemove h id = run h $ sourcesTable # get (expr id) # delete

sourcesInit :: RethinkDBHandle -> IO ()
sourcesInit h = do
    initDb $ run h $ tableCreate sourcesTable
