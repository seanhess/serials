{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Serials.Lib.Auth where

import Prelude hiding (id)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.ByteString hiding (head, last, pack)
import Data.Maybe (fromJust, isJust)
import Safe (headMay)
import Data.Pool (Pool)
import Database.RethinkDB.NoClash (RethinkDBHandle)
import Crypto.BCrypt (validatePassword)
import Network.HTTP.Types
import Network.Wai
import Servant
import Servant.Server.Internal
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Web.JWT (JSON)
import qualified Data.Text as T

import Serials.Lib.JWT
import Serials.Model.User (User (id, hashedPassword), AuthUser(..), SecureUser(..))
import qualified Serials.Model.User as User hiding (User())

data UserLogin = UserLogin {
  email :: Text,
  password :: Text
} deriving (Show, Generic)

instance FromJSON UserLogin
instance ToJSON UserLogin

type TokenLookup = ByteString -> IO Bool

data WithAuthToken

instance HasServer api => HasServer (WithAuthToken :> api) where
    type Server (WithAuthToken :> api) = (TokenLookup, Server api)

    route Proxy (tokenLookup, serv) request respond = case lookup "Authorization" (requestHeaders request) of
      Nothing -> respond . succeedWith $ responseLBS status401 [] ""
      Just x  -> do
        let auth = getAuthToken x
        cookie <- tokenLookup auth
        if cookie
          then route (Proxy :: Proxy api) serv request respond
          else respond . succeedWith $ responseLBS status403 [] ""

getAuthToken :: ByteString -> ByteString
getAuthToken token = encodeUtf8 . last $ T.split (==' ') $ decodeUtf8 token

checkCurrentAuth :: Pool RethinkDBHandle -> Maybe Text -> IO (Maybe User)
checkCurrentAuth h token = case token of
  Nothing -> return Nothing
  Just t -> do
    tok <- verifyJwt t
    case tok of
      Nothing -> return Nothing
      Just to -> User.find h $ pack $ show $ fromJust $ subject tok

checkAuthToken :: Pool RethinkDBHandle -> TokenLookup
checkAuthToken h token = do
  tok <- verifyJwt $ decodeUtf8 token
  case tok of
    Nothing -> return False
    Just x -> return True

userLogin :: Pool RethinkDBHandle -> UserLogin -> IO (Either Text AuthUser)
userLogin h u = do
  users <- User.findByEmail h $ email u
  case headMay users of
    Nothing -> return $ Left "Invalid email address"
    Just user -> do
      let hashPass = encodeUtf8 . fromJust $ User.hashedPassword user
      let pass = encodeUtf8 $ password u
      jwtToken <- signedJwtWebToken $ id user
      case validatePassword hashPass pass of
        False -> return $ Left "Invalid password"
        True -> return $ Right $ AuthUser (SecureUser user) jwtToken

