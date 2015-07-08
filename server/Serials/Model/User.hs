{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Serials.Model.User where

import Prelude hiding (id)

import Control.Applicative

import Data.Aeson (ToJSON, FromJSON, Value(..), toJSON, parseJSON, object, (.=), (.:), (.:?), gToJSON)
import Data.Text (Text, unpack, pack, toLower)
import qualified Data.ByteString.Char8 as C
import Data.Maybe (catMaybes, fromJust)
import qualified Data.HashMap.Strict as HashMap
import Data.Pool
import Data.Time

import Control.Monad.IO.Class (liftIO)
import Control.Monad (mzero)

import Safe (headMay)
import Web.JWT (JSON)

import GHC.Generics (from, Generic)
import qualified Database.RethinkDB.NoClash as R
import Database.RethinkDB.NoClash hiding (table, Object, toJSON)

import Serials.Model.Lib.Crud
import Serials.Model.Types (EmailAddress(..))
import Serials.Lib.JWT

import Serials.AppMonad

import Web.JWT (JWTClaimsSet)

data User = User {
  id :: Text,
  firstName :: Text,
  lastName :: Text,
  email :: EmailAddress,
  hashedPassword :: Text,
  resetToken :: Maybe Text,
  admin :: Bool,
  created :: UTCTime
} deriving (Show, Generic)

instance FromJSON User where
instance ToJSON User where

instance FromDatum User
instance ToDatum User where

-- use SecureUser when you want to hide the hashedPassword
newtype SecureUser = SecureUser User deriving (Show, Generic)

instance ToJSON SecureUser where
  toJSON (SecureUser user) = Object $ foldr HashMap.delete obj ["resetToken", "hashedPassword"]
    where (Object obj) = toJSON user


table = R.table "users"

emailIndexName = "email"
emailIndex = Index emailIndexName

resetIndexName = "resetToken"
resetIndex = Index resetIndexName

list :: App [User]
list = runDb $ table # orderBy [asc "id"]

find :: Text -> App (Maybe User)
find id = runDb $ table # get (expr id)

save ::Text -> User -> App ()
save = docsSave table

findByEmail :: Text -> App (Maybe User)
findByEmail email = do
  us <- runDb $ table # getAll emailIndex [expr email]
  return $ headMay us

remove :: Text -> App ()
remove id = runDb $ table # get (expr id) # delete

secure :: (Functor m) => m User -> m SecureUser
secure = fmap SecureUser

insert :: User -> App User
insert u = do
    r <- runDb $ table # create u
    let user = u {id = generatedKey r}
    return $ user

--------------------------------------------------

addResetToken :: EmailAddress -> Text -> App ()
addResetToken (EmailAddress e) token = runDb $ table # getAll emailIndex [expr e] # update (const ["resetToken" := expr token])

findByToken :: Text -> App (Maybe User)
findByToken token = do
  us <- runDb $ table # getAll resetIndex [expr token]
  return $ headMay us

--------------------------------------------------

init :: App ()
init = do
    initDb $ runDb $ tableCreate table
    initDb $ runDb $ table # indexCreate (emailIndexName) (!expr emailIndexName)
    initDb $ runDb $ table # indexCreate (resetIndexName) (!expr resetIndexName)
