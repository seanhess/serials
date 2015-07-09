{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE MultiParamTypeClasses  #-}

module Serials.Route.Auth where

import Prelude hiding (id, exp)

import Crypto.BCrypt (validatePassword)
import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either
import Control.Monad.Except

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
import Servant.Server.Internal.Enter
import Serials.Lib.ServantCookie
import Serials.AppMonad

import Serials.Lib.JWT
import Serials.Model.User (User (id, hashedPassword), SecureUser(..), secure)
import qualified Serials.Model.User as User hiding (User())
import Serials.Model.App (readAllEnv)

import Web.JWT (JSON, JWTClaimsSet(..), claims, unregisteredClaims, Secret, secret)
import Web.Cookie

import           Data.Typeable (Typeable)
import           GHC.TypeLits  (Symbol)

type AuthToken = Cookie "token" Text

data UserLogin = UserLogin {
  email :: Text,
  password :: Text
} deriving (Show, Generic)

instance FromJSON UserLogin
instance ToJSON UserLogin

data Connected a = Connected (Pool RethinkDBHandle) a

-------------------------------------------------------------------

--class BasicAuthLookup lookup where
    --type BasicAuthVal
    --basicAuthLookup :: Proxy lookup -> IO (Maybe BasicAuthVal)

--type AuthLookup = JWTClaimsSet -> Bool

-- | Basic Authentication with respect to a specified @realm@ and a @lookup@
-- type to encapsulate authentication logic.
--
-- Example:
-- >>> type MyApi = BasicAuth "book-realm" DB :> "books" :> Get '[JSON] [Book]
data Auth lookup deriving (Typeable)

class AuthLookup lookup where
  type AuthVal lookup
  authLookup :: Proxy lookup -> JWTClaimsSet -> IO (Maybe (AuthVal lookup))

-- if you get this far it already works
-- must be an admin!
data RoleAdmin
instance AuthLookup RoleAdmin where
    type AuthVal RoleAdmin = Bool
    authLookup _ c = return $
      if hasClaimAdmin c
      then Just True
      else Nothing

instance (HasServer sublayout, AuthLookup lookup) => HasServer (Auth lookup :> sublayout) where

    type ServerT (Auth lookup :> sublayout) m = ServerT sublayout m

    route _ action request respond =

      case parseToken $ fmap decodeUtf8 $ lookup "Cookie" (requestHeaders request) of
        Nothing -> respond . succeedWith $ responseLBS status401 [] "Missing cookie."
        Just v  -> do
          mcs <- verifyClaims v
          case mcs of
            Nothing -> respond . succeedWith $ responseLBS status403 [] "Invalid auth token."
            Just claims -> do
              maybeAuthData <- authLookup (Proxy :: Proxy lookup) claims
              case maybeAuthData of
                Nothing -> respond . succeedWith $ responseLBS status403 [] "Forbidden"
                -- oh, I see I could pass something in here
                -- (action ad)
                Just ad -> route (Proxy :: Proxy sublayout) action request respond

--protected :: AuthLookup -> server -> (AuthLookup, server)
--protected look server = (look, server)

--instance (Enter typ arg ret) => Enter (AuthLookup, rest) where
    --enter (al, rest) = enter rest

--instance (Enter typ1 arg1 ret1) => Enter (AuthProtected, typ1) arg1 (AuthProtected, ret1) where
    --enter e (a, b) = (a, enter e b)

-- make it only allow an admin for now
--instance HasServer rest => HasServer (AuthProtected :> rest) where
  --type ServerT (AuthProtected :> rest) m = ServerT (AuthLookup, rest) m

  --route Proxy (authLookup, a) request respond = do
    --case parseToken $ fmap decodeUtf8 $ lookup "Cookie" (requestHeaders request) of
      --Nothing -> respond . succeedWith $ responseLBS status401 [] "Missing cookie."
      --Just v  -> do
        --mcs <- verifyClaims v
        --case mcs of
          --Nothing -> respond . succeedWith $ responseLBS status403 [] "Invalid auth token."
          --Just cs -> do
            --if authLookup cs
              --then route (Proxy :: Proxy rest) a request respond
              --else respond . succeedWith $ responseLBS status403 [] "Forbidden"

-----------------------------------------------------------------------

checkCurrentAuth :: Maybe Text -> App (Maybe User)
checkCurrentAuth mjwt = case mjwt of
  Nothing -> return Nothing
  Just jwt -> do
    secret <- liftIO $ jwtSecret
    mt <- liftIO $ verifyJwt secret jwt -- IO
    case mt of -- Maybe
      Nothing -> return Nothing
      Just t  -> do
        case subject t of -- Maybe
          Nothing -> return Nothing
          Just s  -> do
            User.find $ pack $ show s

hasClaimAdmin :: JWTClaimsSet -> Bool
hasClaimAdmin cs = case Map.lookup "admin" $ unregisteredClaims cs of
                     (Just (Aeson.Bool True)) -> True
                     _ -> False

userLogin :: UserLogin -> App (Either Text User)
userLogin u = do
  mu <- User.findByEmail $ toLower $ email u
  case mu of
    Nothing -> return $ Left "Invalid email address"
    Just user -> do
      let hashPass = encodeUtf8 $ User.hashedPassword user
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

addAuth :: User -> App (Headers CookieHeader SecureUser)
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

checkAuth :: Maybe Text -> App (Maybe SecureUser)
checkAuth mt = do
    secure <$> checkCurrentAuth mt

----------------------------------------------------------------

-- copied from timerep, which wouldn't install :( Stupid cabal
formatTimeRFC822 :: UTCTime -> Text
formatTimeRFC822 = pack . formatTime defaultTimeLocale "%a, %d %b %Y %X %z"

------------------------------------------------------------------

currentUser :: Maybe Text -> App SecureUser
currentUser mt = do
  mu <- checkAuth mt
  case mu of
    Nothing -> throwError $ err401 { errBody = "Unauthorized" }
    Just user -> return $ user
