{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Serials.Api where

import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either

import Data.Aeson
import Data.Monoid
import Data.Proxy
import Data.Text (Text, toUpper, unpack)
import Data.Maybe (fromJust)
import Data.Pool
import Data.ByteString.Lazy.Char8 (pack)

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

-- if you use (Maybe a) with liftIO it will return null instead of a 404
-- this is intentional for some routes

-- we only read / write JSON
type Get    a  = Servant.Get    '[JSON] a
type Post   a  = Servant.Post   '[JSON] a
type Put    a  = Servant.Put    '[JSON] a
type Delete a  = Servant.Delete '[JSON] a
type ReqBody a = Servant.ReqBody '[JSON] a

type API =

       "sources" :> Get [Source]
  :<|> "sources" :> ReqBody Source :> Post Text

  :<|> "sources" :> Capture "id" Text :> Get Source
  :<|> "sources" :> Capture "id" Text :> ReqBody Source :> Put ()

  :<|> "sources" :> Capture "id" Text :> "chapters" :> Get [Chapter]
  :<|> "sources" :> Capture "id" Text :> "chapters" :> Post ()
  :<|> "sources" :> Capture "id" Text :> "chapters" :> Delete ()

  :<|> "chapters" :> Capture "id" Text :> Get Chapter
  :<|> "chapters" :> Capture "id" Text :> ReqBody Chapter :> Put ()
  :<|> "chapters" :> Capture "id" Text :> Delete ()

  :<|> "users" :> Capture "id" Text :> Get SecureUser
  :<|> "users" :> Capture "id" Text :> "books" :> Get [Source]

  :<|> "users" :> Capture "id" Text :> "subs" :> Get [Subscription]
  :<|> "users" :> Capture "id" Text :> "subs" :> Capture "id" Text :> Get (Maybe Subscription)
  :<|> "users" :> Capture "id" Text :> "subs" :> Capture "id" Text :> ReqBody Subscription :> Put ()
  :<|> "users" :> Capture "id" Text :> "subs" :> Capture "id" Text :> Post ()
  :<|> "users" :> Capture "id" Text :> "subs" :> Capture "id" Text :> Delete ()

  :<|> "signup" :> ReqBody UserSignup :> Post AuthUser
  :<|> "login" :> ReqBody UserLogin :> Post AuthUser
  :<|> "auth" :> "current" :> QueryParam "token" Text :> Get SecureUser

  :<|> "beta-signup" :> ReqBody BetaSignup :> Post Text

  -- :<|> "read" :> Capture "id" Text :> Raw
  :<|> "proxy" :> Raw

  -- We need to have this prefixed for the time being because of how HasServer instances work in 0.2
  -- 0.3 should have more ability to customize this stuff to our liking
  :<|> "admin" :> AuthProtected :> AdminAPI

  :<|> Raw

type AdminAPI =
       "private" :> Get String -- example to see a few routes in here. Remove when we add more.
  :<|> "import-log" :> Capture "n" Int :> Get Admin.Log

api :: Proxy API
api = Proxy

adminServer :: Pool RethinkDBHandle -> Server AdminAPI
adminServer h = getPrivate :<|> importLog

  where

  getPrivate = return "Serials Private Route"

  importLog n = liftIO $ Admin.importLog n

  --checkAuth = checkAuthToken h

type Handler a = EitherT ServantErr IO a

server :: Pool RethinkDBHandle -> Server API
server h =

  sourcesGetAll :<|> sourcesPost

   :<|> sourcesGet :<|> sourcesPut
   :<|> chaptersGet :<|> sourceScan :<|> chaptersDel

   :<|> chapterGet  :<|> chapterPut :<|> chapterDel

   :<|> userGet
   :<|> userBooksGet
   :<|> userSubsGet :<|> userSubGet :<|> userSubPut :<|> userSubPost :<|> userSubDel

   :<|> signup :<|> login :<|> authCurrent

   :<|> betaSignup
   :<|> proxyApp
   :<|> (checkAuthToken h, adminServer h)
   :<|> serveDirectory "web"

  where

  --appInfo = return $ AppInfo "Serials" "0.1.0"

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


  chapterGet :: Text -> Handler Chapter
  chapterGet id   = liftE $ Chapter.find h id

  chapterPut :: Text -> Chapter -> Handler ()
  chapterPut id c = liftE $ Chapter.save h c

  chapterDel :: Text -> Handler ()
  chapterDel id   = liftIO $ Chapter.remove h id


  userGet :: Text -> Handler SecureUser
  userGet id   = liftE $ secure <$> User.find h id

  userBooksGet :: Text -> Handler [Source]
  userBooksGet uid = liftIO $ Subscription.booksByUser h uid


  userSubsGet :: Text -> Handler [Subscription]
  userSubsGet uid        = liftIO $ Subscription.subsByUser h uid

  userSubGet :: Text -> Text -> Handler (Maybe Subscription)
  userSubGet uid sid     = liftIO $ Subscription.find h uid sid

  userSubPut :: Text -> Text -> Subscription -> Handler ()
  userSubPut uid sid sub = liftIO $ Subscription.save h uid sid sub

  userSubPost :: Text -> Text -> Handler ()
  userSubPost uid sid    = liftIO $ Subscription.add h uid sid

  userSubDel :: Text -> Text -> Handler ()
  userSubDel uid sid     = liftIO $ Subscription.remove h uid sid

  signup :: UserSignup -> Handler AuthUser
  signup u = liftE $ User.insert h u

  login :: UserLogin -> Handler AuthUser
  login u = liftE $ userLogin h u

-- I have several maybes in a row

  authCurrent :: Maybe Text -> Handler SecureUser
  authCurrent mt = liftE $ secure <$> checkCurrentAuth h mt

  betaSignup :: BetaSignup -> Handler Text
  betaSignup b = liftIO $ BetaSignup.insert h b


stack :: Application -> Application
stack app = heads $ cors' $ app
  where
    heads = addHeaders [("X-Source", "Contribute at http://github.com/seanhess/serials")]
    cors' = cors (const $ Just corsResourcePolicy)


-- Run ---------------------------------------------------------

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
  toStatus (Left e) = Left $ err500 { errBody = "Server Error: " <> pack (show e) }
  toStatus (Right v) = Right v

liftE :: ToStatus a => IO (a v) -> EitherT ServantErr IO v
liftE action = EitherT $ toStatus <$> action

