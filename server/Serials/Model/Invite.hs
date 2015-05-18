{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Serials.Model.Invite where

import Prelude hiding (id, lookup, dropWhile, filter, drop)

import Control.Applicative
import Control.Monad

import Data.Aeson (ToJSON, FromJSON, Value(..), (.:), parseJSON)
import Data.Char (intToDigit, chr)
import Data.Text (Text, toLower, unpack, pack)
import Data.Text.Encoding
import Data.Pool
import Data.Monoid ((<>))

import GHC.Generics
import qualified Database.RethinkDB.NoClash as R
import Database.RethinkDB.NoClash hiding (table, Object)

import Safe (headMay)
import Serials.Model.Lib.Crud
import System.Random (randomIO, randomRIO)

import Numeric (showIntAtBase)

type Email = Text

data Invite = Invite {
  id :: Text,
  -- not necessarily the same email they end up signing up with
  -- but we only want to add each email once
  email :: Email,
  code :: Text
} deriving (Show, Generic, Eq)

instance FromJSON Invite
instance ToJSON Invite
instance FromDatum Invite
instance ToDatum Invite

invite :: Email -> IO Invite
invite e = do
  code <- generateCode
  let id = emailId e
  return $ Invite id id code

----------------------------------------------o

codeIndexName = "code"
codeIndex     = Index codeIndexName

table = R.table "invites"

addEmail :: Pool RethinkDBHandle -> Email -> IO ()
addEmail h e = do
    inv <- invite e
    runPool h $ table # insert (toDatum $ inv) :: IO ()

emailId :: Text -> Email
emailId = toLower

all :: Pool RethinkDBHandle -> IO [Invite]
all h = runPool h $ table

find :: Pool RethinkDBHandle -> Text -> IO (Maybe Invite)
find h code = do
    is <- runPool h $ table # getAll codeIndex [expr code]
    return $ headMay is

init :: Pool RethinkDBHandle -> IO ()
init h = do
    initDb $ runPool h $ tableCreate table
    initDb $ runPool h $ table # indexCreate (unpack codeIndexName) (!expr codeIndexName)

--------------------------------------
-- code

-- 0 - 36. produces 0-9,a-z
intCodeToChar :: Int -> Char
intCodeToChar n
  | n < 10    = intToDigit n
  | otherwise = chr (n+87)

intToCode :: Int -> Text
intToCode n = pack $ showIntAtBase 36 intCodeToChar n ""

generateCode :: IO Text
generateCode = do
  n <- randomRIO (0, maxBound)
  return $ intToCode n

