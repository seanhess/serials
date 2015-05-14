{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Serials.Lib.Auth where

import Prelude hiding (id)

import Crypto.BCrypt (validatePassword)
import Control.Applicative ((<$>))

import Data.Text (Text, pack, toLower)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.ByteString hiding (head, last, pack)
import Data.Maybe (fromJust, isJust)
import Data.Pool (Pool)
import Data.Aeson (FromJSON, ToJSON)
import Debug.Trace
import Data.Monoid ((<>))

import Database.RethinkDB.NoClash (RethinkDBHandle)

import GHC.Generics
import Network.HTTP.Types
import Network.Wai

import Safe (headMay)
import Servant
import Servant.Server.Internal

import Web.JWT (JSON)

import Serials.Lib.JWT
import Serials.Model.User (User (id, hashedPassword), AuthUser(..), SecureUser(..))
import qualified Serials.Model.User as User hiding (User())

data UserLogin = UserLogin {
  email :: Text,
  password :: Text
} deriving (Show, Generic)

instance FromJSON UserLogin
instance ToJSON UserLogin

type TokenLookup = Text -> IO Bool

data AuthProtected

data Connected a = Connected (Pool RethinkDBHandle) a

 --how could you give it the connection pool?
instance HasServer rest => HasServer (AuthProtected :> rest) where
  type ServerT (AuthProtected :> rest) m = (TokenLookup, ServerT rest m)

  route Proxy (tokenLookup, a) request respond =
    case lookup "Authorization" (requestHeaders request) of
      Nothing -> respond . succeedWith $ responseLBS status401 [] "Missing auth header."
      Just v  -> do
        let auth = getAuthToken $ decodeUtf8 v
        granted <- tokenLookup auth
        if granted
          then route (Proxy :: Proxy rest) a request respond
          else respond . succeedWith $ responseLBS status403 [] "Invalid auth token."

getAuthToken :: Text -> Text
getAuthToken token = last $ T.split (==' ') $ token

checkCurrentAuth :: Pool RethinkDBHandle -> Maybe Text -> IO (Maybe User)
checkCurrentAuth h mjwt = case mjwt of
  Nothing -> return Nothing
  Just jwt -> do
    mt <- verifyJwt jwt -- IO
    case mt of -- Maybe
      Nothing -> return Nothing
      Just t  -> do
        case subject t of -- Maybe
          Nothing -> return Nothing
          Just s  -> do
            User.find h $ pack $ show s

checkAuthToken :: Pool RethinkDBHandle -> TokenLookup
checkAuthToken h token = do
  tok <- verifyJwt token
  case tok of
    Nothing -> return False
    Just x -> return True

userLogin :: Pool RethinkDBHandle -> UserLogin -> IO (Either Text AuthUser)
userLogin h u = do
  users <- User.findByEmail h $ toLower $ email u
  case headMay users of
    Nothing -> return $ Left "Invalid email address"
    Just user -> do
      let hashPass = encodeUtf8 . fromJust $ User.hashedPassword user
      let pass = encodeUtf8 $ password u
      jwtToken <- signedJwtWebToken $ id user
      case validatePassword hashPass pass of
        False -> return $ Left "Invalid password"
        True -> return $ Right $ AuthUser (SecureUser user) jwtToken

