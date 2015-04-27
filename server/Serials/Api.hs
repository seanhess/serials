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
import Data.Text (Text, toUpper, unpack)
import Data.Maybe (fromJust)
import Data.Pool

import GHC.Generics

import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp (run, Port)
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.AddHeaders

import Serials.Model.Source (Source(..))
import qualified Serials.Model.Source as Source
import qualified Serials.Model.Chapter as Chapter
import Serials.Model.Chapter (Chapter(..))
import Serials.Model.App
import Serials.Model.Crud (initDb, runPool)
import Serials.Scan

--import Web.Scotty
import Servant

import Database.RethinkDB.NoClash (RethinkDBHandle, use, db, connect, RethinkDBError, Datum)
import qualified Database.RethinkDB as R

type API =

       "sources" :> Get [Source]
  :<|> "sources" :> ReqBody Source :> Post Text

  :<|> "sources" :> Capture "id" Text :> Get Source
  :<|> "sources" :> Capture "id" Text :> ReqBody Source :> Put ()
  :<|> "sources" :> Capture "id" Text :> Delete

  :<|> "sources" :> Capture "id" Text :> "chapters" :> Get [Chapter]
  :<|> "sources" :> Capture "id" Text :> "chapters" :> Post ()
  :<|> "sources" :> Capture "id" Text :> "chapters" :> Delete

  :<|> "chapters" :> Capture "id" Text :> Get Chapter
  :<|> "chapters" :> Capture "id" Text :> ReqBody Chapter :> Put ()

  :<|> Raw

api :: Proxy API
api = Proxy

server :: Pool RethinkDBHandle -> Server API
server h = 
         sourcesGetAll :<|> sourcesPost 
    :<|> sourcesGet :<|> sourcesPut :<|> sourcesDel
    :<|> chaptersGet :<|> sourceScan :<|> chaptersDel
    :<|> chapterGet  :<|> chapterPut
    :<|> serveDirectory "web" 

  where 

  --appInfo = return $ AppInfo "Serials" "0.1.0"

  sourcesGetAll = liftIO $ Source.list h
  sourcesPost s = liftIO $ Source.insert h s

  sourcesGet id   = liftE  $ Source.find h id
  sourcesPut id s = liftIO $ Source.save h id s
  sourcesDel id   = liftIO $ Source.remove h id

  chaptersGet id = liftIO $ Chapter.bySource h id
  chaptersDel id = liftIO $ Chapter.deleteBySource h id
  sourceScan  id = liftE  $ importSource h id

  chapterGet id   = liftE $ Chapter.find h id
  chapterPut id c = liftE  $ Chapter.save h c


stack :: Application -> Application
stack app = heads $ cors' $ app
  where
    heads = addHeaders [("X-Source", "Contribute at http://github.com/seanhess/serials")]
    cors' = cors (const $ Just corsResourcePolicy)


-- Run ---------------------------------------------------------

runApi :: Int -> (String, Integer) -> IO ()
runApi port dbHost = do
    p <- connectDbPool dbHost
    createDb p
    Source.init p
    Chapter.init p
    putStrLn $ "Starting..."
    run port $ stack $ serve api (server p)
    return ()

-- DB -----------------------------------------------------------

-- I could just read it here.. it's easy
connectDb :: (String,Integer) -> IO RethinkDBHandle
connectDb (host,port) = use serialsDb <$> connect host port Nothing

disconnectDb :: RethinkDBHandle -> IO ()
disconnectDb h = R.close h

connectDbPool :: (String, Integer) -> IO (Pool RethinkDBHandle)
connectDbPool hp = createPool (connectDb hp) disconnectDb 1 10 5

createDb :: Pool RethinkDBHandle -> IO ()
createDb p = initDb $ runPool p $ R.dbCreate $ unpack serialsDbName

serialsDb = db serialsDbName
serialsDbName = "serials"

-- Cors ---------------------------------------------------------

corsResourcePolicy :: CorsResourcePolicy
corsResourcePolicy = CorsResourcePolicy
    { corsOrigins = Nothing
    , corsMethods = ["GET", "HEAD", "OPTIONS", "POST", "PUT", "DELETE"]
    , corsRequestHeaders = simpleResponseHeaders
    , corsExposedHeaders = Nothing
    , corsMaxAge = Nothing
    , corsVaryOrigin = False
    , corsRequireOrigin = False
    , corsIgnoreFailures = False
    }

-- ToStatus ------------------------------------------------------

class ToStatus a where
    toStatus :: a val -> Either (Int, String) val

instance ToStatus Maybe where
    toStatus Nothing  = Left (404, "Not Found")
    toStatus (Just v) = Right v

instance Show a => ToStatus (Either a) where
    toStatus (Left e) = Left (500, "Server Error: " <> show e)
    toStatus (Right v) = Right v

liftE :: ToStatus a => IO (a v) -> EitherT (Int, String) IO v
liftE action = EitherT $ toStatus <$> action