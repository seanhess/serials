{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Serials.Api where

import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either

import Data.Aeson
import Data.Monoid
import Data.Proxy
import Data.Text (Text, toUpper, unpack, pack)
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL
import Data.Maybe (fromJust)
import Data.Pool
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

import GHC.Generics

import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp (run, Port)
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.AddHeaders

import Serials.Model.Source (Source(..))
import Serials.Model.Chapter (Chapter(..))
import Serials.Model.User (User(..), AuthUser, SecureUser, secure)
import Serials.Model.UserSignup (UserSignup)
import Serials.Model.BetaSignup (BetaSignup(..))
import Serials.Model.Subscription (Subscription(..))
import qualified Serials.Model.Source as Source
import qualified Serials.Model.Chapter as Chapter
import qualified Serials.Model.User as User
import qualified Serials.Model.BetaSignup as BetaSignup
import qualified Serials.Model.Subscription as Subscription
import Serials.Model.App
import Serials.Lib.Auth (UserLogin, AuthProtected, checkAuthToken, userLogin, checkCurrentAuth, Connected(..))
import Serials.Model.Lib.Crud
import Serials.Scan
import qualified Serials.Admin as Admin
import Serials.Read.Test (proxyApp)

import Servant hiding (Get, Post, Put, Delete, ReqBody)
import qualified Servant

import Database.RethinkDB.NoClash (RethinkDBHandle, use, db, connect, RethinkDBError, Datum)
import qualified Database.RethinkDB as R

import Web.Cookie

-- if you use (Maybe a) with liftIO it will return null instead of a 404
-- this is intentional for some routes

-- we only read / write JSON
type Get    a  = Servant.Get    '[JSON] a
type Post   a  = Servant.Post   '[JSON] a
type Put    a  = Servant.Put    '[JSON] a
type Delete a  = Servant.Delete '[JSON] a
type ReqBody a = Servant.ReqBody '[JSON] a

type Handler a = EitherT ServantErr IO a


-- Chapters ---------------------------------------------------
type ChaptersAPI =
       Capture "id" Text :> Get Chapter
  :<|> Capture "id" Text :> ReqBody Chapter :> Put ()
  :<|> Capture "id" Text :> Delete ()

chaptersServer :: Pool RethinkDBHandle -> Server ChaptersAPI
chaptersServer h =
  chapterGet  :<|> chapterPut :<|> chapterDel

  where
  chapterGet :: Text -> Handler Chapter
  chapterGet id   = liftE $ Chapter.find h id

  chapterPut :: Text -> Chapter -> Handler ()
  chapterPut id c = liftE $ Chapter.save h c

  chapterDel :: Text -> Handler ()
  chapterDel id   = liftIO $ Chapter.remove h id




-- Sources -----------------------------------------------------

type SourcesAPI =
       Get [Source]
  :<|> ReqBody Source :> Post Text

  :<|> Capture "id" Text :> Get Source
  :<|> Capture "id" Text :> ReqBody Source :> Put ()

  :<|> Capture "id" Text :> "chapters" :> Get [Chapter]
  :<|> Capture "id" Text :> "chapters" :> Post ()
  :<|> Capture "id" Text :> "chapters" :> Delete ()

sourcesServer :: Pool RethinkDBHandle -> Server SourcesAPI
sourcesServer h =
        sourcesGetAll :<|> sourcesPost
   :<|> sourcesGet :<|> sourcesPut
   :<|> chaptersGet :<|> sourceScan :<|> chaptersDel

  where

  sourcesGetAll :: Handler [Source]
  sourcesGetAll = liftIO $ Source.list h

  sourcesPost :: Source -> Handler Text
  sourcesPost s = liftIO $ Source.insert h s

  sourcesGet :: Text -> Handler Source
  sourcesGet id   = liftE  $ Source.find h id

  sourcesPut :: Text -> Source -> Handler ()
  sourcesPut id s = liftIO $ Source.save h id s

  chaptersGet :: Text -> Handler [Chapter]
  chaptersGet id = liftIO $ Chapter.bySource h id

  chaptersDel :: Text -> Handler ()
  chaptersDel id = liftIO $ Chapter.deleteBySource h id

  sourceScan :: Text -> Handler ()
  sourceScan  id = liftIO $ importSourceId h id




-- Admin -------------------------------------------------------

type AdminAPI =
       "private" :> Get String -- example to see a few routes in here. Remove when we add more.
  :<|> "import-log" :> Capture "n" Int :> Get Admin.Log
  :<|> "test" :> Get Text

adminServer :: Pool RethinkDBHandle -> Server AdminAPI
adminServer h = getPrivate :<|> importLog :<|> test

  where

  getPrivate = return "Serials Private Route"

  importLog n = liftIO $ Admin.importLog n
  test = return "UMMM"


  --checkAuth = checkAuthToken h






-- Users -------------------------------------------------------

type UsersAPI =

       Capture "id" Text :> Get SecureUser
  :<|> Capture "id" Text :> "books" :> Get [Source]

  :<|> Capture "id" Text :> "subs" :> Get [Subscription]
  :<|> Capture "id" Text :> "subs" :> Capture "id" Text :> Get (Maybe Subscription)
  :<|> Capture "id" Text :> "subs" :> Capture "id" Text :> ReqBody Subscription :> Put ()
  :<|> Capture "id" Text :> "subs" :> Capture "id" Text :> Post ()
  :<|> Capture "id" Text :> "subs" :> Capture "id" Text :> Delete ()

usersServer :: Pool RethinkDBHandle -> Server UsersAPI
usersServer h =
        userGet
   :<|> userBooksGet
   :<|> userSubsGet :<|> userSubGet :<|> userSubPut :<|> userSubPost :<|> userSubDel

  where

  userGet :: Text -> Handler SecureUser
  userGet id   = liftE $ secure <$> User.find h id

  userBooksGet :: Text -> Handler [Source]
  userBooksGet uid = liftIO $ Subscription.booksByUser h uid


  userSubsGet :: Text -> Handler [Subscription]
  userSubsGet uid = liftIO $ Subscription.subsByUser h uid

  userSubGet :: Text -> Text -> Handler (Maybe Subscription)
  userSubGet uid sid = liftIO $ Subscription.find h uid sid

  userSubPut :: Text -> Text -> Subscription -> Handler ()
  userSubPut uid sid sub = liftIO $ Subscription.save h uid sid sub

  userSubPost :: Text -> Text -> Handler ()
  userSubPost uid sid = liftIO $ Subscription.add h uid sid

  userSubDel :: Text -> Text -> Handler ()
  userSubDel uid sid = liftIO $ Subscription.remove h uid sid


-- Auth -----------------------------------------------------

type CookieHeader = '[Header "Set-Cookie" Text]

type AuthAPI =

       Header "Cookie" Text :> Get SecureUser
  :<|> Delete (Headers CookieHeader ())
  :<|> ReqBody UserLogin :> Put (Headers CookieHeader AuthUser)

  -- this is more like POST users
  :<|> "signup"  :> ReqBody UserSignup :> Post AuthUser
  :<|> "beta-signup" :> ReqBody BetaSignup :> Post Text

authServer :: Pool RethinkDBHandle -> Server AuthAPI
authServer h = current :<|> logout :<|> login :<|> signup :<|> beta

  where

  signup :: UserSignup -> Handler AuthUser
  signup u = liftE $ User.insert h u

  login :: UserLogin -> Handler (Headers CookieHeader AuthUser)
  login u = liftE $ do
    eu <- userLogin h u
    return $ fmap addAuthHeader eu

  logout :: Handler (Headers CookieHeader ())
  logout = return clearAuthHeader
  -- also, expire the previous token?
  -- what happens on login

  current :: Maybe Text -> Handler SecureUser
  current mc = liftE $ checkAuth h mc

  beta :: BetaSignup -> Handler Text
  beta b = liftIO $ BetaSignup.insert h b



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
checkAuth h mc = secure <$> checkCurrentAuth h (parseToken mc)



-- Server ----------------------------------------------------


type API =

       "sources"  :> SourcesAPI
  :<|> "chapters" :> ChaptersAPI
  :<|> "users"    :> UsersAPI
  :<|> "auth"     :> AuthAPI

  :<|> "proxy" :> Raw

  -- :<|> "admin" :> AuthProtected :> AdminAPI
  :<|> "admin" :> AdminAPI

  :<|> "settings"    :> Header "Cookie" Text :> Get AppSettings
  :<|> "settings.js" :> Header "Cookie" Text :> Servant.Get '[PlainText] Text

  :<|> Raw


server :: Pool RethinkDBHandle -> Server API
server h =

        sourcesServer h
   :<|> chaptersServer h
   :<|> usersServer h
   :<|> authServer h

   :<|> proxyApp
   :<|> adminServer h
   -- :<|> (checkAuthToken h, adminServer h)

   :<|> liftIO . settings
   :<|> settingsText
   :<|> serveDirectory "web"

  where

    settingsText :: Maybe Text -> Handler Text
    settingsText mc = liftIO $ do
      s <- settings mc
      let json = TL.toStrict $ TLE.decodeUtf8 $ encode s :: Text
      return $ "var SETTINGS=" <> (json)

    settings :: Maybe Text -> IO AppSettings
    settings mc = do
      user <- checkAuth h mc
      return $ AppSettings "Serials" "0.2" user "http://localhost:3001"

    printVar :: ToJSON a => Text -> a -> Text
    printVar key a = ""

  --appInfo = return $ AppInfo "Serials" "0.1.0"

data AppSettings = AppSettings {
  appName :: Text,
  version :: Text,
  user :: Maybe SecureUser,
  endpoint :: Text
} deriving (Show, Generic)

instance ToJSON AppSettings



-- Run ---------------------------------------------------------

stack :: Application -> Application
stack app = heads $ cors' $ app
  where
    heads = addHeaders [("X-Source", "Contribute at http://github.com/seanhess/serials")]
    cors' = cors (const $ Just corsResourcePolicy)

api :: Proxy API
api = Proxy


runApi :: Int -> Pool RethinkDBHandle -> IO ()
runApi port p = do
  createDb p
  Source.init p
  Chapter.init p
  User.init p
  BetaSignup.init p
  Subscription.init p
  putStrLn $ "Starting..."
  run port $ stack $ serve api (server p)
  return ()

-- Cors ---------------------------------------------------------

corsResourcePolicy :: CorsResourcePolicy
corsResourcePolicy = CorsResourcePolicy
  { corsOrigins = Nothing
  , corsMethods = ["GET", "HEAD", "OPTIONS", "POST", "PUT", "DELETE"]
  , corsRequestHeaders = simpleResponseHeaders <> ["Authorization"]
  , corsExposedHeaders = Nothing
  , corsMaxAge = Nothing
  , corsVaryOrigin = False
  , corsRequireOrigin = False
  , corsIgnoreFailures = False
  }

-- ToStatus ------------------------------------------------------

class ToStatus a where
  toStatus :: a val -> Either ServantErr val

instance ToStatus Maybe where
  toStatus Nothing  = Left $ err404
  toStatus (Just v) = Right v

instance Show a => ToStatus (Either a) where
  toStatus (Left e) = Left $ err500 { errBody = "Server Error: " <> BL.pack (show e) }
  toStatus (Right v) = Right v

liftE :: ToStatus a => IO (a v) -> EitherT ServantErr IO v
liftE action = EitherT $ toStatus <$> action

