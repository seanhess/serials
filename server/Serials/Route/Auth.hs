{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Serials.Route.Auth where

import Prelude hiding (id, exp)

import Crypto.BCrypt (validatePassword)
import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either

import Data.Text (Text, pack, toLower)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Time
import Data.ByteString hiding (head, last, pack)
import Data.Maybe (fromJust, isJust)
import Data.Pool (Pool)
import Data.Aeson (FromJSON, ToJSON(..))
import qualified Data.Aeson as Aeson
import Debug.Trace
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import qualified Data.Map.Lazy as Map
import Data.Map.Lazy (fromList)

import Database.RethinkDB.NoClash (RethinkDBHandle)

import GHC.Generics
import Network.HTTP.Types hiding (Header)
import Network.Wai

import Safe (headMay)
import Servant
import Servant.Server.Internal
import Serials.Lib.ServantCookie
import Serials.Route.Route

import Serials.Lib.JWT
import Serials.Model.User (User (id, hashedPassword), SecureUser(..), secure)
import qualified Serials.Model.User as User hiding (User())
import Serials.Model.App (readAllEnv, Env(..))

import Web.JWT (JSON, JWTClaimsSet(..), claims, unregisteredClaims, Secret, secret)
import Web.Cookie

type AuthToken = Cookie "token" Text

data UserLogin = UserLogin {
  email :: Text,
  password :: Text
} deriving (Show, Generic)

instance FromJSON UserLogin
instance ToJSON UserLogin

-------------------------------------------------------------------

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

-----------------------------------------------------------------------

checkCurrentAuth :: Pool RethinkDBHandle -> Maybe Text -> IO (Maybe User)
checkCurrentAuth h mjwt = case mjwt of
  Nothing -> return Nothing
  Just jwt -> do
    secret <- jwtSecret
    mt <- verifyJwt secret jwt -- IO
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

userLogin :: Pool RethinkDBHandle -> UserLogin -> IO (Either Text User)
userLogin h u = do
  mu <- User.findByEmail h $ toLower $ email u
  case mu of
    Nothing -> return $ Left "Invalid email address"
    Just user -> do
      let hashPass = encodeUtf8 . fromJust $ User.hashedPassword user
      let pass = encodeUtf8 $ password u
      case validatePassword hashPass pass of
        False -> return $ Left "Invalid password"
        True -> return $ Right user

verifyClaims :: Text -> IO (Maybe JWTClaimsSet)
verifyClaims t = do
    secret <- jwtSecret
    jwt <- verifyJwt secret t
    return $ fmap claims jwt

--userJWT :: User -> IO Text
--userJWT user = do
    --claims <- userClaims user
    --return $ signClaims secret claims

jwtSecret :: IO Secret
jwtSecret = do
    env <- readAllEnv
    return $ secret $ authSecret env

userClaims :: User -> IO JWTClaimsSet
userClaims user = defaultClaims (id user) $ fromList [adminClaim]
  where adminClaim = ("admin", toJSON $ User.admin user)

-- Cookie Auth Stuff  -------------------------------------------


type CookieHeader = '[Header "Set-Cookie" Text]

addAuthHeader :: Secret -> JWTClaimsSet -> a -> Headers CookieHeader a
addAuthHeader secret claims a = addHeader header a
  where
  header = "token=" <> token <> "; path=/; HttpOnly; expires=" <> expires
  token   = signClaims secret claims
  expTime = toUTCTime $ fromJust $ exp claims
  expires = formatTimeRFC822 expTime

addAuth :: User -> Handler (Headers CookieHeader SecureUser)
addAuth u = do
  claims <- liftIO $ userClaims u
  secret <- liftIO $ jwtSecret
  return $ (addAuthHeader secret claims (SecureUser u))

clearAuthHeader :: Headers CookieHeader ()
clearAuthHeader = addHeader ("token=deleted; path=/; expires=Thu, 01 Jan 1970 00:00:00 GMT") ()

parseToken :: Maybe Text -> Maybe Text
parseToken mc = do
  cookie <- mc
  token <- lookup "token" $ parseCookiesText $ encodeUtf8 cookie
  return $ token

checkAuth :: Pool RethinkDBHandle -> Maybe Text -> IO (Maybe SecureUser)
checkAuth h mt = do
    secure <$> checkCurrentAuth h mt

----------------------------------------------------------------

-- copied from timerep, which wouldn't install :( Stupid cabal
formatTimeRFC822 :: UTCTime -> Text
formatTimeRFC822 = pack . formatTime defaultTimeLocale "%a, %d %b %Y %X %z"

------------------------------------------------------------------

currentUser :: Pool RethinkDBHandle -> Maybe Text -> Handler SecureUser
currentUser h mt = do
  mu <- liftIO $ checkAuth h mt
  case mu of
    Nothing -> EitherT $ return $ Left $ err401 { errBody = "Unauthorized" }
    Just user -> return $ user
