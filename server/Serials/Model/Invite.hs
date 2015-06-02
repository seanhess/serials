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
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Time
import Data.Time.ISO8601 (formatISO8601)

import GHC.Generics
import qualified Database.RethinkDB.NoClash as R
import Database.RethinkDB.NoClash hiding (table, Object)
import Database.RethinkDB.Time (now)

import Safe (headMay)
import Serials.Model.Lib.Crud
import System.Random (randomIO, randomRIO)

import Numeric (showIntAtBase)

type Email = Text
type InviteCode = Text

data Invite = Invite {
  id :: Text,
  email :: Email,
  code :: InviteCode,
  -- when they sign up they are here
  userId :: Maybe Text,
  sent :: Maybe UTCTime
} deriving (Show, Generic, Eq)

instance FromJSON Invite
instance ToJSON Invite
instance FromDatum Invite
instance ToDatum Invite

invite :: Email -> Maybe UTCTime -> IO Invite
invite e mt = do
  code <- generateCode
  let id = emailId e
  return $ Invite id id code Nothing mt


----------------------------------------------o

codeIndexName = "code"
codeIndex     = Index codeIndexName

table = R.table "invites"

add :: Pool RethinkDBHandle -> Invite -> IO Invite
add h inv = do
    r <- runPool h $ table # ex insert [returnChanges] (toDatum $ inv)
    return . fromJust $ writeChangeNew r

remove :: Pool RethinkDBHandle -> Text -> IO ()
remove h code = runPool h $ table # getAll codeIndex [expr code] # delete

emailId :: Text -> Email
emailId = toLower

all :: Pool RethinkDBHandle -> IO [Invite]
all h = runPool h $ table

find :: Pool RethinkDBHandle -> InviteCode -> IO (Maybe Invite)
find h code = do
    is <- runPool h $ table # getAll codeIndex [expr code]
    return $ headMay is

markUsed :: Pool RethinkDBHandle -> InviteCode -> Text -> IO ()
markUsed h code userId = runPool h $ table # getAll codeIndex [expr code] # update (const ["userId" := expr userId])

markSent :: Pool RethinkDBHandle -> InviteCode -> IO ()
markSent h code = do
    time <- getCurrentTime
    runPool h $ table # getAll codeIndex [expr code] # update (const ["sent" := formatISO8601 time])

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

