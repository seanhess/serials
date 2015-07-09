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
import Control.Monad.Except
import Control.Monad.Reader

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
import Data.HashMap.Strict (HashMap)

import GHC.Generics

import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp (run, Port)
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.AddHeaders
import Network.Wai.Middleware.Static

import Serials.Model.Source (Source(..), TagCount(..))
import Serials.Model.Chapter (Chapter(..))
import Serials.Model.User (User(..), SecureUser(..), secure)
import Serials.Model.Invite (Invite(..))
import Serials.Model.Types (EmailAddress(..))
import Serials.Model.Subscription (Subscription(..))
import qualified Serials.Model.Source as Source
import qualified Serials.Model.Change as Change
import qualified Serials.Model.Chapter as Chapter
import qualified Serials.Model.User as User
import qualified Serials.Model.Invite as Invite
import qualified Serials.Model.Subscription as Subscription
import Serials.Model.App
import Serials.Model.Lib.Crud
import Serials.Scan
import qualified Serials.Admin as Admin
import Serials.Lib.Mail (Email(..))

import Serials.Types
import Serials.Route.App
import Serials.Route.Auth
import Serials.Route.Invite
import Serials.Route.UserSignup
import Serials.Route.Sources
import qualified Serials.Route.UserSignup as UserSignup

import Servant hiding (Get, Post, Put, Delete, ReqBody)
import qualified Servant
import Serials.Lib.ServantCookie

import Database.RethinkDB.NoClash (RethinkDBHandle, use, db, connect, RethinkDBError, Datum)
import qualified Database.RethinkDB as R

import Web.JWT (JWTClaimsSet)
import Web.Scotty
import Text.Blaze.Html.Renderer.Text (renderHtml)







-- Admin -------------------------------------------------------

type AdminAPI =
       "private" :> Get String -- example to see a few routes in here. Remove when we add more.
  :<|> "import-log" :> Capture "n" Int :> Get Admin.Log

adminServer :: ServerT AdminAPI App
adminServer = getPrivate :<|> importLog

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

  :<|> Auth RoleAdmin :> AdminUserAPI

type AdminUserAPI =
       Get [SecureUser]
  :<|> Capture "id" Text :> Delete ()

adminUsersServer :: ServerT AdminUserAPI App
adminUsersServer = usersGet :<|> usersDel

  where

  usersGet :: App [SecureUser]
  usersGet = secure <$> User.list

  usersDel :: Text -> App ()
  usersDel id = User.remove id


usersServer :: ServerT UsersAPI App
usersServer =
        signup
   :<|> userGet
   :<|> userBooksGet
   :<|> userSubsGet :<|> userSubGet :<|> userSubPut :<|> userSubPost :<|> userSubDel

    -- this sucks. It has to be last or it "protects" everything in here. 
   :<|> adminUsersServer

  where

-- what does that mean?
  signup :: UserSignup -> App (Headers CookieHeader SecureUser)
  signup s = do
    u <- checkErrorText err400 $ UserSignup.signup s
    addAuth u

  userGet :: Text -> App SecureUser
  userGet id = checkNotFound $ secure <$> User.find id

  userBooksGet :: Text -> App [Source]
  userBooksGet uid = Subscription.booksByUser uid

  userSubsGet :: Text -> App [Subscription]
  userSubsGet uid = Subscription.subsByUser uid

  userSubGet :: Text -> Text -> App (Maybe Subscription)
  userSubGet uid sid = Subscription.find uid sid

  userSubPut :: Text -> Text -> Subscription -> App ()
  userSubPut uid sid sub = Subscription.save uid sid sub

  userSubPost :: Text -> Text -> App ()
  userSubPost uid sid = Subscription.add uid sid

  userSubDel :: Text -> Text -> App ()
  userSubDel uid sid = Subscription.remove uid sid


-- Auth -----------------------------------------------------

type AuthAPI =

       AuthToken :> Get SecureUser
  :<|> Delete (Headers CookieHeader ())
  :<|> ReqBody UserLogin :> Put (Headers CookieHeader SecureUser)

  :<|> "jwt"     :> AuthToken :> Get (Maybe JWTClaimsSet)

  :<|> "password" :> ReqBody EmailAddress :> Post ()
  :<|> "password" :> Capture "token" Text :> ReqBody Text :> Post ()


authServer :: ServerT AuthAPI App
authServer = current :<|> logout :<|> login :<|> jwt :<|> forgot :<|> reset

  where

  login :: UserLogin -> App (Headers CookieHeader SecureUser)
  login u = do
    u <- checkErrorText err401 $ userLogin u
    addAuth u

  -- TODO expire the users' token
  logout :: App (Headers CookieHeader ())
  logout = return clearAuthHeader

  current :: Maybe Text -> App SecureUser
  current mt = checkNotFound $ checkAuth mt

  jwt mt = liftIO $ case mt of
    Nothing -> return Nothing
    Just t  -> verifyClaims t

  forgot :: EmailAddress -> App ()
  forgot email = forgotPassword email

  reset :: Text -> Text -> App()
  reset token pw = checkErrorText err400 $ resetPassword token pw

-- Invites ----------------------------------------

type InvitesAPI =
       Get [Invite]
  :<|> ReqBody EmailAddress :> Post ()
  :<|> Capture "code" Text :> Get Invite
  :<|> Capture "code" Text :> Delete ()
  :<|> Capture "code" Text :> "sent" :> Post ()

invitesServer :: ServerT InvitesAPI App
invitesServer = list :<|> add :<|> find :<|> remove :<|> send

  where

  add :: EmailAddress -> App ()
  add e = checkErrorText err400 $ inviteAddEmail e

  list :: App [Invite]
  list = Invite.all

  find :: Text -> App Invite
  find code = checkNotFound $ Invite.find code

  remove :: Text -> App ()
  remove code = Invite.remove code

  send :: Text -> App ()
  send code = inviteSend code


type ExampleAPI = Auth RoleAdmin :> Get String

exampleServerT :: Application -> ServerT ExampleAPI App
exampleServerT root = hello
  where
  hello = return "Hello"

exampleServer :: AppConfig -> Application -> Server ExampleAPI
exampleServer config root = enter (Nat $ (runApp config)) (exampleServerT root)


-- Server ----------------------------------------------------

type MainAPI =
  "status" :> Get AppStatus

  :<|> "sources"   :> SourcesAPI
  :<|> "changes"     :> ChangesAPI
  :<|> "users"     :> UsersAPI
  :<|> "auth"      :> AuthAPI
  :<|> "invites"     :> InvitesAPI

  :<|> "admin"       :> Auth RoleAdmin :> AdminAPI

  :<|> "tags"        :> Get [TagCount]
  :<|> "settings"    :> AuthToken :> Get AppSettings
  :<|> "settings.js" :> AuthToken :> Servant.Get '[PlainText] Text

apiServer :: ServerT MainAPI App
apiServer =

   status

   :<|> sourcesServer
   :<|> changesServer

   :<|> usersServer
   :<|> authServer
   :<|> invitesServer

   :<|> adminServer

   :<|> getTags

   :<|> settings
   :<|> settingsText

  where

    settingsText :: Maybe Text -> App Text
    settingsText mc = do
      s <- settings mc
      let json = TL.toStrict $ TLE.decodeUtf8 $ encode s :: Text
      return $ "var SETTINGS=" <> (json)

    settings :: Maybe Text -> App AppSettings
    settings mc = do
      user <- checkAuth mc
      e    <- asks env
      v    <- asks version
      return $ AppSettings "Serials" v user (endpoint e) (environment e)

    printVar :: ToJSON a => Text -> a -> Text
    printVar key a = ""

    status = appStatus

    getTags = Source.allTags

server :: AppConfig -> Server MainAPI
server config = enter (Nat $ (runApp config)) apiServer

-- (serverApp root)
-- (runAppT config)

------------------------------------------------------------

type RootAPI = MainAPI :<|> Raw

rootServer :: AppConfig -> Application -> Server RootAPI
rootServer config root = server config :<|> root

api :: Proxy RootAPI
api = Proxy

-----------------------------------------------------------

rootApp :: AppConfig -> IO Application
rootApp c = scottyApp $ do
  middleware $ staticPolicy (noDots >-> addBase "web")

  get "/app"   $ file "./web/app.html"
  get "/hello" $ file "./web/hello.html"
  get "/"      $ file "./web/index.html"

  -- test to see what emails look like
  --get "/emails/invite" $ do
    ---- get an invite, the first one?
    ---- I can't actually call these without the entire environment, right?
    --res <- liftIO $ runAppT
    --invs <- liftIO $ runAppT c $ Invite.all
    --env <- liftIO $ readAllEnv
    --let (Email _ body) = inviteEmail (head invs) (endpoint env)
    --html $ renderHtml body

  --get "/emails/welcome" $ do
    ---- get an invite, the first one?
    --us <- liftIO $ runAppT c $ User.list
    --env <- liftIO $ readAllEnv
    --let (Email _ body) = UserSignup.welcomeEmail (endpoint env) (head us)
    --html $ renderHtml body


data AppSettings = AppSettings {
  appName :: Text,
  appVersion :: Text,
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

runApi :: AppConfig -> IO ()
runApi config = do

  let p = port $ env config
  root <- rootApp config

  res <- runEitherT $ runApp config $ do
    createDb
    Source.init
    Change.init
    User.init
    Invite.init
    Subscription.init

  putStrLn $ "Starting..."
  run p $ stack $ serve api (rootServer config root)
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

