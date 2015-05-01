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
import Data.Maybe (catMaybes, fromJust, listToMaybe)
import Data.Pool
import Data.Time
import Crypto.BCrypt

import GHC.Generics
import qualified Database.RethinkDB.NoClash as R
import Database.RethinkDB.NoClash hiding (table, Object)

import Serials.Model.UserSignupFields (UserSignupFields)
import qualified Serials.Model.UserSignupFields as U
import Serials.Lib.Crud
import Serials.Lib.Helpers

data User = User {
  id :: Text
  , firstName :: Text
  , lastName :: Text
  , email :: Text

  , hashedPassword :: Maybe Text

  , token :: Text
  , admin :: Bool -- Should we have more permissions?

  , created :: UTCTime
} deriving (Show, Generic)

instance FromJSON User where
  parseJSON (Object v) = User
    <$> v .: "id"
    <*> v .: "firstName"
    <*> v .: "lastName"
    <*> v .: "email"
    <*> v .:? "hashedPassword"
    <*> v .: "token"
    <*> v .: "admin"
    <*> v .: "created"
  parseJSON _ = mzero

instance ToJSON User where
  toJSON (User id firstName lastName email hashedPassword token admin created) = object $ catMaybes
    [ ("id" .=) <$> pure id
      , ("firstName" .=) <$> pure firstName
      , ("lastName" .=) <$> pure lastName
      , ("email" .=) <$> pure email
      , ("token" .=) <$> pure token
      , ("admin" .=) <$> pure admin
      , ("created" .=) <$> pure created
    ]

instance FromDatum User
instance ToDatum User where
  toDatum (User id firstName lastName email hashedPassword token admin created) = toDatum . object $ catMaybes
    [ ("id" .=) <$> pure id
      , ("firstName" .=) <$> pure firstName
      , ("lastName" .=) <$> pure lastName
      , ("email" .=) <$> pure email
      , ("hashedPassword" .=) <$> hashedPassword
      , ("token" .=) <$> pure token
      , ("admin" .=) <$> pure admin
      , ("created" .=) <$> pure created
    ]

table = R.table "users"

tokenIndexName = "token"
tokenIndex = Index tokenIndexName

emailIndexName = "email"
emailIndex = Index emailIndexName

list :: Pool RethinkDBHandle -> IO [User]
list h = runPool h $ table

find :: Pool RethinkDBHandle -> Text -> IO (Maybe User)
find h id = runPool h $ table # get (expr id)

findByToken :: Pool RethinkDBHandle -> Text -> IO [User]
findByToken h token = runPool h $ table # getAll tokenIndex [expr token]

findByEmail :: Pool RethinkDBHandle -> Text -> IO [User]
findByEmail h email = runPool h $ table # getAll emailIndex [expr email]

{-insertUser :: User -> Maybe Text -> Either Text (Maybe User)-}
{-insertUser j (Just x) = Right . Just $ j { id = unpack x }-}
{-insertUser _ Nothing = Left WriteFailure-}

insert :: Pool RethinkDBHandle -> UserSignupFields -> IO (Either Text User)
insert h u = do
    let pass = U.password u
    if pass == (U.passwordConfirmation u)
      then do
        hashPass <- liftIO . hashPasswordUsingPolicy customHashPolicy . fromString $ unpack pass
        isEmail <- findByEmail h $ U.email u
        created <- liftIO getCurrentTime
        token <- liftIO randomString
        let user = User {
          id = pack ""
          , firstName = U.firstName u
          , lastName = U.lastName u
          , email = U.email u
          , hashedPassword = Just . pack $ toString $ fromJust hashPass
          , token = pack token
          , admin = False
          , created = created
        }
        case listToMaybe isEmail of
          Nothing -> do
            r <- runPool h $ table # create user
            return . Right $ user {id = generatedKey r}
          Just _-> return $ Left "User already exists with that email"
    else return $ Left "Password and Password Confirmation do not match"

init :: Pool RethinkDBHandle -> IO ()
init h = do
    initDb $ runPool h $ tableCreate table
    initDb $ runPool h $ table # indexCreate (unpack tokenIndexName) (!expr tokenIndexName)
    initDb $ runPool h $ table # indexCreate (unpack emailIndexName) (!expr emailIndexName)

