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
import Serials.Model.User (User(..), SecureUser(..), secure)
import Serials.Model.Invite (Invite(..), Email)
import Serials.Model.Subscription (Subscription(..))
import qualified Serials.Model.Source as Source
import qualified Serials.Model.Chapter as Chapter
import qualified Serials.Model.User as User
import qualified Serials.Model.Invite as Invite
import qualified Serials.Model.Subscription as Subscription
import Serials.Model.App
import Serials.Model.Lib.Crud
import Serials.Scan
import qualified Serials.Admin as Admin
import Serials.Read.Test (proxyApp)

import Serials.Route.Auth
import Serials.Route.Invite
import Serials.Route.UserSignup (UserSignup)
import qualified Serials.Route.UserSignup as UserSignup

import Servant hiding (Get, Post, Put, Delete, ReqBody)
import qualified Servant
import Serials.Lib.ServantCookie

import Database.RethinkDB.NoClash (RethinkDBHandle, use, db, connect, RethinkDBError, Datum)
import qualified Database.RethinkDB as R

import Web.JWT (JWTClaimsSet)


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

adminServer :: Pool RethinkDBHandle -> Server AdminAPI
adminServer h = getPrivate :<|> importLog

  where

  getPrivate = return "Serials Private Route"

  importLog n = liftIO $ Admin.importLog n







-- Users -------------------------------------------------------

type UsersAPI =

       ReqBody UserSignup :> Post (Headers CookieHeader SecureUser)

  :<|> Capture "id" Text :> Get SecureUser
  :<|> Capture "id" Text :> "books" :> Get [Source]

  :<|> Capture "id" Text :> "subs" :> Get [Subscription]
  :<|> Capture "id" Text :> "subs" :> Capture "id" Text :> Get (Maybe Subscription)
  :<|> Capture "id" Text :> "subs" :> Capture "id" Text :> ReqBody Subscription :> Put ()
  :<|> Capture "id" Text :> "subs" :> Capture "id" Text :> Post ()
  :<|> Capture "id" Text :> "subs" :> Capture "id" Text :> Delete ()

usersServer :: Pool RethinkDBHandle -> Server UsersAPI
usersServer h =
        signup
   :<|> userGet
   :<|> userBooksGet
   :<|> userSubsGet :<|> userSubGet :<|> userSubPut :<|> userSubPost :<|> userSubDel


  where

  signup :: UserSignup -> Handler (Headers CookieHeader SecureUser)
  signup s = do
    u <- liftErrText err400 $ liftIO $ UserSignup.signup h s
    addAuth u

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

type AuthToken = Cookie "token" Text

type AuthAPI =

       AuthToken :> Get SecureUser
  :<|> Delete (Headers CookieHeader ())
  :<|> ReqBody UserLogin :> Put (Headers CookieHeader SecureUser)

  :<|> "jwt"     :> AuthToken :> Get (Maybe JWTClaimsSet)

addAuth :: User -> Handler (Headers CookieHeader SecureUser)
addAuth u = do
  token <- liftIO $ userJWT u
  return $ (addAuthHeader token (SecureUser u))

authServer :: Pool RethinkDBHandle -> Server AuthAPI
authServer h = current :<|> logout :<|> login :<|> jwt

  where

  login :: UserLogin -> Handler (Headers CookieHeader SecureUser)
  login u = do
    u <- liftErrText err401 $ liftIO $ userLogin h u
    addAuth u

  -- TODO expire the users' token
  logout :: Handler (Headers CookieHeader ())
  logout = return clearAuthHeader

  current :: Maybe Text -> Handler SecureUser
  current mt = liftE $ checkAuth h mt

  jwt mt = liftIO $ case mt of
    Nothing -> return Nothing
    Just t  -> verifyClaims t

-- Invites ----------------------------------------

type InvitesAPI =
       Get [Invite]
  :<|> ReqBody Email :> Post ()
  :<|> Capture "id" Text :> Get Invite
  :<|> Capture "id" Text :> "sent" :> Post ()

invitesServer :: Pool RethinkDBHandle -> Server InvitesAPI
invitesServer h = list :<|> add :<|> find :<|> send

  where

  add :: Email -> Handler ()
  add e = liftIO $ inviteAddEmail h e

  list :: Handler [Invite]
  list = liftIO $ Invite.all h

  find :: Text -> Handler Invite
  find code = liftE $ Invite.find h code

  send :: Text -> Handler ()
  send code = liftIO $ inviteSend h code



-- Server ----------------------------------------------------



type API =

       "sources"  :> SourcesAPI
  :<|> "chapters" :> ChaptersAPI
  :<|> "users"    :> UsersAPI
  :<|> "auth"     :> AuthAPI
  :<|> "invites"  :> InvitesAPI

  :<|> "proxy" :> Raw

  :<|> "admin" :> AuthProtected :> AdminAPI

  :<|> "settings"    :> AuthToken :> Get AppSettings
  :<|> "settings.js" :> AuthToken :> Servant.Get '[PlainText] Text
  :<|> "status" :> Get AppStatus

  :<|> Raw


server :: Pool RethinkDBHandle -> Env -> Server API
server h env =

        sourcesServer h
   :<|> chaptersServer h
   :<|> usersServer h
   :<|> authServer h
   :<|> invitesServer h

   :<|> proxyApp
   :<|> protected hasClaimAdmin (adminServer h)

   :<|> liftIO . settings
   :<|> settingsText

   :<|> status

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
      return $ AppSettings "Serials" "0.2" user (envEndpoint env)

    printVar :: ToJSON a => Text -> a -> Text
    printVar key a = ""

    status = liftIO $ appStatus h

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


data Env = Env {
  port :: Int,
  db :: (String, Integer),
  mandrill :: Text,
  envEndpoint :: Text
} deriving (Show)

runApi :: Int -> Pool RethinkDBHandle -> Env -> IO ()
runApi port p env = do
  createDb p
  Source.init p
  Chapter.init p
  User.init p
  Invite.init p
  Subscription.init p
  putStrLn $ "Starting..."
  run port $ stack $ serve api (server p env)
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



errUnauthorized :: Text -> ServantErr
errUnauthorized err = err401 { errReasonPhrase = unpack err }

errText :: ServantErr -> Text -> ServantErr
errText err txt = err { errBody = TLE.encodeUtf8 $ TL.fromStrict txt }

liftErr :: (err -> ServantErr) -> IO (Either err res) -> EitherT ServantErr IO res
liftErr toErr action = do
    eres <- liftIO action
    case eres of
      Left err -> left (toErr err)
      Right res -> return res

liftErrText :: ServantErr -> IO (Either Text res) -> EitherT ServantErr IO res
liftErrText err = liftErr (errText err)
