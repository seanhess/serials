{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Serials.Route.Auth where

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
import qualified Data.Aeson as Aeson
import Debug.Trace
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import qualified Data.Map.Lazy as Map

import Database.RethinkDB.NoClash (RethinkDBHandle)

import GHC.Generics
import Network.HTTP.Types hiding (Header)
import Network.Wai

import Safe (headMay)
import Servant
import Servant.Server.Internal

import Serials.Lib.JWT
import Serials.Model.User (User (id, hashedPassword), AuthUser(..), SecureUser(..), secure, userJWT)
import qualified Serials.Model.User as User hiding (User())

import Web.JWT (JSON, JWTClaimsSet, claims, unregisteredClaims)
import Web.Cookie


data UserLogin = UserLogin {
  email :: Text,
  password :: Text
} deriving (Show, Generic)

instance FromJSON UserLogin
instance ToJSON UserLogin

type AuthLookup = JWTClaimsSet -> Bool

data AuthProtected

data Connected a = Connected (Pool RethinkDBHandle) a

protected :: AuthLookup -> server -> (AuthLookup, server)
protected look server = (look, server)

-- make it only allow an admin for now
instance HasServer rest => HasServer (AuthProtected :> rest) where
  type ServerT (AuthProtected :> rest) m = (AuthLookup, ServerT rest m)

  route Proxy (authLookup, a) request respond = do
    case parseToken $ fmap decodeUtf8 $ lookup "Cookie" (requestHeaders request) of
      Nothing -> respond . succeedWith $ responseLBS status401 [] "Missing cookie."
      Just v  -> do
        mcs <- verifyClaims v
        case mcs of
          Nothing -> respond . succeedWith $ responseLBS status403 [] "Invalid auth token."
          Just cs -> do
            if authLookup cs
              then route (Proxy :: Proxy rest) a request respond
              else respond . succeedWith $ responseLBS status403 [] "Forbidden"

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

hasClaimAdmin :: JWTClaimsSet -> Bool
hasClaimAdmin cs = case Map.lookup "admin" $ unregisteredClaims cs of
                     (Just (Aeson.Bool True)) -> True
                     _ -> False

userLogin :: Pool RethinkDBHandle -> UserLogin -> IO (Either Text AuthUser)
userLogin h u = do
  users <- User.findByEmail h $ toLower $ email u
  case headMay users of
    Nothing -> return $ Left "Invalid email address"
    Just user -> do
      let hashPass = encodeUtf8 . fromJust $ User.hashedPassword user
      let pass = encodeUtf8 $ password u
      jwt <- userJWT user
      case validatePassword hashPass pass of
        False -> return $ Left "Invalid password"
        True -> return $ Right $ AuthUser (SecureUser user) jwt

verifyClaims :: Text -> IO (Maybe JWTClaimsSet)
verifyClaims t = do
    jwt <- verifyJwt t
    return $ fmap claims jwt


-- Cookie Auth Stuff  -------------------------------------------

type CookieHeader = '[Header "Set-Cookie" Text]

addAuthHeader :: AuthUser -> Headers CookieHeader AuthUser
addAuthHeader auth = addHeader ("token=" <> User.token auth <> "; path=/; HttpOnly;") auth

clearAuthHeader :: Headers CookieHeader ()
clearAuthHeader = addHeader ("token=deleted; path=/; expires=Thu, 01 Jan 1970 00:00:00 GMT") ()

parseToken :: Maybe Text -> Maybe Text
parseToken mc = do
  cookie <- mc
  token <- lookup "token" $ parseCookiesText $ encodeUtf8 cookie
  return $ token

checkAuth :: Pool RethinkDBHandle -> Maybe Text -> IO (Maybe SecureUser)
checkAuth h mt = secure <$> checkCurrentAuth h mt
