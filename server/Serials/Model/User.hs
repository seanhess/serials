{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Serials.Model.User where

import Prelude hiding (id)

import Control.Applicative

import Data.Aeson (ToJSON, FromJSON, Value(..), toJSON, parseJSON, object, (.=), (.:), (.:?), gToJSON)
import Data.Text (Text, unpack, pack, toLower)
import Data.ByteString.UTF8 (fromString, toString)
import qualified Data.ByteString.Char8 as C
import Data.Maybe (catMaybes, fromJust)
import qualified Data.HashMap.Strict as HashMap
import Data.Pool
import Data.Time

import Control.Monad.IO.Class (liftIO)
import Control.Monad (mzero)
import Crypto.BCrypt

import Safe (headMay)
import Web.JWT (JSON)

import GHC.Generics (from, Generic)
import qualified Database.RethinkDB.NoClash as R
import Database.RethinkDB.NoClash hiding (table, Object, toJSON)

import Serials.Model.UserSignup (UserSignup)
import qualified Serials.Model.UserSignup as U
import Serials.Model.Lib.Crud
import Serials.Lib.JWT

import Web.JWT (JWTClaimsSet)

data User = User {
  id :: Text,
  firstName :: Text,
  lastName :: Text,
  email :: Text,
  hashedPassword :: Maybe Text,
  admin :: Bool,
  created :: UTCTime
} deriving (Show, Generic)

instance FromJSON User where
instance ToJSON User where

instance FromDatum User
instance ToDatum User where

-- use SecureUser when you want to hide the hashedPassword
newtype SecureUser = SecureUser User deriving (Show, Generic)

instance FromJSON SecureUser
instance ToJSON SecureUser where
  toJSON (SecureUser user) = Object $ HashMap.delete "hashedPassword" obj
    where (Object obj) = toJSON user

table = R.table "users"

emailIndexName = "email"
emailIndex = Index emailIndexName

list :: Pool RethinkDBHandle -> IO [User]
list h = runPool h $ table

find :: Pool RethinkDBHandle -> Text -> IO (Maybe User)
find h id = runPool h $ table # get (expr id)

findByEmail :: Pool RethinkDBHandle -> Text -> IO [User]
findByEmail h email = runPool h $ table # getAll emailIndex [expr email]

secure :: (Functor m) => m User -> m SecureUser
secure = fmap SecureUser

insert :: Pool RethinkDBHandle -> UserSignup -> IO (Either Text User)
insert h u = do
    let pass = U.password u
    if pass == (U.passwordConfirmation u)
      then do
        hashPass <- liftIO . hashPasswordUsingPolicy customHashPolicy . fromString $ unpack pass
        isEmail <- findByEmail h $ toLower $ U.email u
        created <- liftIO getCurrentTime
        let user' = User {
          id             = pack "",
          firstName      = U.firstName u,
          lastName       = U.lastName u,
          email          = U.email u,
          hashedPassword = Just . pack $ toString $ fromJust hashPass,
          admin          = False,
          created        = created
        }
        case headMay isEmail of
          Nothing -> do
            r <- runPool h $ table # create user'
            let user = user' {id = generatedKey r}
            return . Right $ user
          Just _-> return $ Left "User already exists with that email"
    else return $ Left "Password and Password Confirmation do not match"

init :: Pool RethinkDBHandle -> IO ()
init h = do
    initDb $ runPool h $ tableCreate table
    initDb $ runPool h $ table # indexCreate (unpack emailIndexName) (!expr emailIndexName)

customHashPolicy :: HashingPolicy
customHashPolicy = HashingPolicy 10 (C.pack "$2b$")

