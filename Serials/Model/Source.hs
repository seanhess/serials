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


-- how could I preconfigure it with a table?

-- find :: RethinkDBHandle -> Text -> IO (Maybe a)
-- find h id = run h $ table # get (expr id)

-- give me a table, give me a rethinkdb connection
-- and I'll give you a bunch of IO functions you can run :)
-- crud :: Table -> RethinkDBHandle -> IO a


-- I want IO actions, in case I need to do other ones
-- and to fit nicely, right?
-- hmm.. no... the controller actions should tie things together, right?
-- these really can be explicitly database operations
-- this module exports database operations
-- which means that RethinkIO makes sense
-- but maybe not having them run it.. I run it?

--sourcesList :: RethinkDB IO [Source]
--sourcesList = table "sources"

--sourcesFind :: Text -> RethinkDB IO (Maybe Source)
--sourcesFind id = table "sources" # get (expr id)

--newtype RethinkDB m a = RethinkDB { runRethink :: m a }
-- I want a monad transformer!
-- yay!
-- it's secretly a reader!
-- what's the point then?
-- it would be rad if you could still do IO operations, right?
-- but you can't


