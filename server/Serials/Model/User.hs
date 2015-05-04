{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Serials.Model.User where

import Prelude hiding (id)

import Control.Applicative

import Data.Text (Text, unpack, pack)
import Data.ByteString.UTF8 (fromString, toString)
import Data.Aeson (ToJSON, FromJSON, Value(..), toJSON, parseJSON, object, (.=), (.:), (.:?))
import Control.Monad.IO.Class (liftIO)
import Control.Monad (mzero)
import Data.Maybe (catMaybes, fromJust)
import Safe (headMay)
import Data.Pool
import Data.Time
import Crypto.BCrypt
import qualified Data.ByteString.Char8 as C

import GHC.Generics
import qualified Database.RethinkDB.NoClash as R
import Database.RethinkDB.NoClash hiding (table, Object)

import Serials.Model.UserSignup (UserSignup)
import qualified Serials.Model.UserSignup as U
import Serials.Model.Lib.Crud

data User = User {
  id :: Text
  , firstName :: Text
  , lastName :: Text
  , email :: Text
  , hashedPassword :: Maybe Text
  , admin :: Bool
  , created :: UTCTime
  } deriving (Show, Generic)

instance FromJSON User where
  parseJSON (Object v) = User
    <$> v .: "id"
    <*> v .: "firstName"
    <*> v .: "lastName"
    <*> v .: "email"
    <*> v .:? "hashedPassword"
    <*> v .: "admin"
    <*> v .: "created"
  parseJSON _ = mzero

instance ToJSON User where
  toJSON (User id firstName lastName email hashedPassword admin created) = object $ catMaybes
    [ ("id" .=) <$> pure id
    , ("firstName" .=) <$> pure firstName
    , ("lastName" .=) <$> pure lastName
    , ("email" .=) <$> pure email
    , ("admin" .=) <$> pure admin
    , ("created" .=) <$> pure created
    ]

instance FromDatum User
instance ToDatum User where
  toDatum (User id firstName lastName email hashedPassword admin created) = toDatum . object $ catMaybes
    [ ("id" .=) <$> pure id
    , ("firstName" .=) <$> pure firstName
    , ("lastName" .=) <$> pure lastName
    , ("email" .=) <$> pure email
    , ("hashedPassword" .=) <$> hashedPassword
    , ("admin" .=) <$> pure admin
    , ("created" .=) <$> pure created
    ]

table = R.table "users"

emailIndexName = "email"
emailIndex = Index emailIndexName

list :: Pool RethinkDBHandle -> IO [User]
list h = runPool h $ table

find :: Pool RethinkDBHandle -> Text -> IO (Maybe User)
find h id = runPool h $ table # get (expr id)

findByEmail :: Pool RethinkDBHandle -> Text -> IO [User]
findByEmail h email = runPool h $ table # getAll emailIndex [expr email]

insert :: Pool RethinkDBHandle -> UserSignup -> IO (Either Text User)
insert h u = do
    let pass = U.password u
    if pass == (U.passwordConfirmation u)
      then do
        hashPass <- liftIO . hashPasswordUsingPolicy customHashPolicy . fromString $ unpack pass
        isEmail <- findByEmail h $ U.email u
        created <- liftIO getCurrentTime
        let user = User {
          id = pack ""
          , firstName = U.firstName u
          , lastName = U.lastName u
          , email = U.email u
          , hashedPassword = Just . pack $ toString $ fromJust hashPass
          , admin = False
          , created = created
        }
        case headMay isEmail of
          Nothing -> do
            r <- runPool h $ table # create user
            return . Right $ user {id = generatedKey r}
          Just _-> return $ Left "User already exists with that email"
    else return $ Left "Password and Password Confirmation do not match"

init :: Pool RethinkDBHandle -> IO ()
init h = do
    initDb $ runPool h $ tableCreate table
    initDb $ runPool h $ table # indexCreate (unpack emailIndexName) (!expr emailIndexName)

customHashPolicy :: HashingPolicy
customHashPolicy = HashingPolicy 10 (C.pack "$2b$")

