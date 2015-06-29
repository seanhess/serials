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

type EmailAddress = Text
type InviteCode = Text

data Signup = Signup {
  userId :: Text,
  date :: UTCTime
} deriving (Show, Generic, Eq)

data Invite = Invite {
  id :: Text,
  email :: EmailAddress,
  code :: InviteCode,
  -- when they sign up they are here
  signup :: Maybe Signup,
  sent :: Maybe UTCTime,
  created :: UTCTime
} deriving (Show, Generic, Eq)


instance ToJSON Signup
instance FromJSON Signup

instance ToDatum Signup

instance ToJSON Invite
instance FromJSON Invite

instance FromDatum Invite
instance ToDatum Invite

invite :: EmailAddress -> IO Invite
invite e = do
  code <- generateCode
  time <- getCurrentTime
  let id = emailId e
  return $ Invite id id code Nothing Nothing time


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

emailId :: Text -> EmailAddress
emailId = toLower

all :: Pool RethinkDBHandle -> IO [Invite]
all h = runPool h $ table # orderBy [asc "id"]

find :: Pool RethinkDBHandle -> InviteCode -> IO (Maybe Invite)
find h code = do
    is <- runPool h $ table # getAll codeIndex [expr code]
    return $ headMay is

markUsed :: Pool RethinkDBHandle -> InviteCode -> Text -> IO ()
markUsed h code userId = do
  time <- getCurrentTime
  let signup = Signup userId time
  runPool h $ table # getAll codeIndex [expr code] # update (const ["signup" := toDatum signup])

markSent :: Pool RethinkDBHandle -> InviteCode -> IO ()
markSent h code = do
    time <- getCurrentTime
    runPool h $ table # getAll codeIndex [expr code] # update (const ["sent" := formatISO8601 time])

init :: Pool RethinkDBHandle -> IO ()
init h = do
    initDb $ runPool h $ tableCreate table
    initDb $ runPool h $ table # indexCreate (codeIndexName) (!expr codeIndexName)

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

