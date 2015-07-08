{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Serials.Model.Invite where

import Prelude hiding (id, lookup, dropWhile, filter, drop)

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (liftIO)

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
import Serials.Model.Types (EmailAddress(..))
import System.Random (randomIO, randomRIO)

import Serials.AppMonad

import Numeric (showIntAtBase)

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
  return $ Invite id (EmailAddress id) code Nothing Nothing time

----------------------------------------------

codeIndexName = "code"
codeIndex     = Index codeIndexName

table = R.table "invites"

add :: Invite -> App Invite
add inv = do
    r <- runDb $ table # ex insert [returnChanges] (toDatum $ inv)
    return . fromJust $ writeChangeNew r

remove :: Text -> App ()
remove code = runDb $ table # getAll codeIndex [expr code] # delete

emailId :: EmailAddress -> Text
emailId (EmailAddress e) = toLower e

all :: App [Invite]
all = runDb $ table # orderBy [asc "id"]

find :: InviteCode -> App (Maybe Invite)
find code = do
    is <- runDb $ table # getAll codeIndex [expr code]
    return $ headMay is

markUsed :: InviteCode -> Text -> App ()
markUsed code userId = do
  time <- liftIO $ getCurrentTime
  let signup = Signup userId time
  runDb $ table # getAll codeIndex [expr code] # update (const ["signup" := toDatum signup])

markSent :: InviteCode -> App ()
markSent code = do
    time <- liftIO $ getCurrentTime
    runDb $ table # getAll codeIndex [expr code] # update (const ["sent" := formatISO8601 time])

init :: App ()
init = do
    initDb $ runDb $ tableCreate table
    initDb $ runDb $ table # indexCreate (codeIndexName) (!expr codeIndexName)

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

