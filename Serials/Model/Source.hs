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


data Source = Source {
  id :: Maybe Text,
  sourceUrl :: Text,
  sourceName :: Text
} deriving (Show, Generic)

instance FromJSON Source
instance ToJSON Source
instance FromDatum Source
instance ToDatum Source

sourcesTable = table "sources"

sourcesList :: RethinkDBHandle -> IO [Source]
sourcesList h = run h $ table "sources"

sourcesFind :: RethinkDBHandle -> Text -> IO (Maybe Source)
sourcesFind h id = run h $ table "sources" # get (expr id)

sourcesCreate :: RethinkDBHandle -> Source -> IO Text
sourcesCreate h s = do
    r <- run h $ table "sources" # create s
    return $ generatedKey r

sourcesSave :: RethinkDBHandle -> Text -> Source -> IO ()
sourcesSave h id s = run h $ table "sources" # get (expr id) # replace (const (toDatum s))

sourcesRemove :: RethinkDBHandle -> Text -> IO ()
sourcesRemove h id = run h $ table "sources" # get (expr id) # delete

