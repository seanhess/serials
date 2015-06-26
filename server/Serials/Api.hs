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
import Data.Maybe (fromJust)
import Data.Pool
import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL

import GHC.Generics

import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp (run, Port)
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.AddHeaders
import Network.Wai.Middleware.Static

import Serials.Model.Source (Source(..))
import Serials.Model.Submission (Submission(..))
import Serials.Model.Chapter (Chapter(..))
import Serials.Model.User (User(..), SecureUser(..), secure)
import Serials.Model.Invite (Invite(..), EmailAddress)
import Serials.Model.Subscription (Subscription(..))
import qualified Serials.Model.Source as Source
import qualified Serials.Model.Chapter as Chapter
import qualified Serials.Model.User as User
import qualified Serials.Model.Invite as Invite
import qualified Serials.Model.Subscription as Subscription
import qualified Serials.Model.Submission as Submission
import Serials.Model.App
import Serials.Model.Lib.Crud
import Serials.Scan
import qualified Serials.Admin as Admin
import Serials.Read.Test (proxyApp)
import Serials.Lib.Mail (Email(..))

import Serials.Route.Route
import Serials.Route.Auth
import Serials.Route.Invite
import Serials.Route.UserSignup (UserSignup)
import Serials.Route.Sources (SourcesAPI, sourcesServer, ChangesAPI, changesServer)
import qualified Serials.Route.UserSignup as UserSignup

import Servant hiding (Get, Post, Put, Delete, ReqBody)
import qualified Servant
import Serials.Lib.ServantCookie

import Database.RethinkDB.NoClash (RethinkDBHandle, use, db, connect, RethinkDBError, Datum)
import qualified Database.RethinkDB as R

import Web.JWT (JWTClaimsSet)
import Web.Scotty
import Text.Blaze.Html.Renderer.Text (renderHtml)





-- Submissions ---------------------------------------------------

--type SubmissionsAPI =
       --Get [Submission]
  -- :<|> ReqBody Submission :> Post Text

  -- :<|> Capture "id" Text :> Get Submission
  -- :<|> Capture "id" Text :> ReqBody Submission :> Put ()

--proposalsServer :: Pool RethinkDBHandle -> Server SubmissionsAPI
--proposalsServer h =
        --proposalsGetAll :<|> proposalsPost
   -- :<|> proposalsGet :<|> proposalsPut

  --where

  --proposalsGetAll :: Handler [Submission]
  --proposalsGetAll = liftIO $ Submission.list h

  --proposalsPost :: Submission -> Handler Text
  --proposalsPost s = liftIO $ Submission.insert h s

  --proposalsGet :: Text -> Handler Submission
  --proposalsGet id   = liftE  $ Submission.find h id

  --proposalsPut :: Text -> Submission -> Handler ()
  --proposalsPut id s = liftIO $ Submission.save h id s


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

  :<|> AuthProtected :> AdminUserAPI

type AdminUserAPI =
       Get [SecureUser]
  :<|> Capture "id" Text :> Delete ()

adminUsersServer :: Pool RethinkDBHandle -> Server AdminUserAPI
adminUsersServer h = usersGet :<|> usersDel

  where

  usersGet :: Handler [SecureUser]
  usersGet = liftIO $ secure <$> User.list h

  usersDel :: Text -> Handler ()
  usersDel id = liftIO $ User.remove h id


usersServer :: Pool RethinkDBHandle -> Server UsersAPI
usersServer h =
        signup
   :<|> userGet
   :<|> userBooksGet
   :<|> userSubsGet :<|> userSubGet :<|> userSubPut :<|> userSubPost :<|> userSubDel

    -- this sucks. It has to be last or it "protects" everything in here. 
   :<|> protected hasClaimAdmin (adminUsersServer h)

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

type AuthAPI =

       AuthToken :> Get SecureUser
  :<|> Delete (Headers CookieHeader ())
  :<|> ReqBody UserLogin :> Put (Headers CookieHeader SecureUser)

  :<|> "jwt"     :> AuthToken :> Get (Maybe JWTClaimsSet)

addAuth :: User -> Handler (Headers CookieHeader SecureUser)
addAuth u = do
  claims <- liftIO $ userClaims u
  return $ (addAuthHeader claims (SecureUser u))

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
  :<|> ReqBody EmailAddress :> Post ()
  :<|> Capture "code" Text :> Get Invite
  :<|> Capture "code" Text :> Delete ()
  :<|> Capture "code" Text :> "sent" :> Post ()

invitesServer :: Pool RethinkDBHandle -> Server InvitesAPI
invitesServer h = list :<|> add :<|> find :<|> remove :<|> send

  where

  add :: EmailAddress -> Handler ()
  add e = EitherT $ inviteAddEmail h e

  list :: Handler [Invite]
  list = liftIO $ Invite.all h

  find :: Text -> Handler Invite
  find code = liftE $ Invite.find h code

  remove :: Text -> Handler ()
  remove code = liftIO $ Invite.remove h code

  send :: Text -> Handler ()
  send code = liftIO $ inviteSend h code



-- Server ----------------------------------------------------

type API =

       "sources"   :> SourcesAPI
  :<|> "changes"   :> ChangesAPI
  :<|> "users"     :> UsersAPI
  :<|> "auth"      :> AuthAPI
  :<|> "invites"   :> InvitesAPI

  :<|> "proxy" :> Raw

  :<|> "admin" :> AuthProtected :> AdminAPI

  :<|> "settings"    :> AuthToken :> Get AppSettings
  :<|> "settings.js" :> AuthToken :> Servant.Get '[PlainText] Text
  :<|> "status" :> Get AppStatus

  :<|> Raw

server :: Pool RethinkDBHandle -> String -> Env -> Application -> Server API
server h version env root =

        sourcesServer h
   :<|> changesServer h

   :<|> usersServer h
   :<|> authServer h
   :<|> invitesServer h

   :<|> proxyApp
   :<|> protected hasClaimAdmin (adminServer h)

   :<|> liftIO . settings
   :<|> settingsText

   :<|> status

   :<|> root

  where

    settingsText :: Maybe Text -> Handler Text
    settingsText mc = liftIO $ do
      s <- settings mc
      let json = TL.toStrict $ TLE.decodeUtf8 $ encode s :: Text
      return $ "var SETTINGS=" <> (json)

    settings :: Maybe Text -> IO AppSettings
    settings mc = do
      user <- checkAuth h mc
      return $ AppSettings "Serials" version user (endpoint env) (environment env)

    printVar :: ToJSON a => Text -> a -> Text
    printVar key a = ""

    status = liftIO $ appStatus h

rootApp :: Pool RethinkDBHandle -> IO Application
rootApp h = scottyApp $ do
  middleware $ staticPolicy (noDots >-> addBase "web")

  get "/app"   $ file "./web/app.html"
  get "/hello" $ file "./web/hello.html"
  get "/"      $ file "./web/index.html"

  -- test to see what emails look like
  get "/emails/invite" $ do
    -- get an invite, the first one?
    invs <- liftIO $ Invite.all h
    env <- liftIO $ readAllEnv
    let (Email _ body) = inviteEmail (head invs) (endpoint env)
    html $ renderHtml body

  get "/emails/welcome" $ do
    -- get an invite, the first one?
    us <- liftIO $ User.list h
    env <- liftIO $ readAllEnv
    let (Email _ body) = UserSignup.welcomeEmail (endpoint env) (head us)
    html $ renderHtml body



data AppSettings = AppSettings {
  appName :: Text,
  version :: String,
  user :: Maybe SecureUser,
  appEndpoint :: Text,
  appEnvironment :: AppEnvironment
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

runApi :: Int -> Pool RethinkDBHandle -> String -> Env -> IO ()
runApi port p version env = do
  env <- readAllEnv
  root <- rootApp p
  createDb p
  Source.init p
  Submission.init p
  User.init p
  Invite.init p
  Subscription.init p
  putStrLn $ "Starting..."
  run port $ stack $ serve api (server p version env root)
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

