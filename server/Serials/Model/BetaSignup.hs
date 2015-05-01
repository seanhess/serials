{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Serials.Model.BetaSignup where

import Prelude hiding (id, lookup, dropWhile, filter, drop)

import Data.Text (Text)
import Data.Aeson (ToJSON, FromJSON)
import Data.Pool

import GHC.Generics
import qualified Database.RethinkDB.NoClash as R
import Database.RethinkDB.NoClash hiding (table)

import Serials.Model.Lib.Crud

import Numeric

-- these are the things you can change
-- they need an id if you're going to send them down :(
data BetaSignup = BetaSignup {
  id :: Text
  , email :: Text
} deriving (Show, Generic, Eq)

instance FromJSON BetaSignup
instance ToJSON BetaSignup
instance FromDatum BetaSignup
instance ToDatum BetaSignup

table = R.table "betaSignups"

insert :: Pool RethinkDBHandle -> BetaSignup -> IO Text
insert h s = do
    r <- runPool h $ table # create s
    return $ generatedKey r

init :: Pool RethinkDBHandle -> IO ()
init h = initDb $ runPool h $ tableCreate table

