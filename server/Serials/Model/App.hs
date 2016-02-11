{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Serials.Model.App where

import Control.Applicative
import Control.Monad.IO.Class (liftIO)

import Data.Aeson
import Data.Pool
import Data.Text (Text, pack)
import Data.Maybe
import Data.Time

import Database.RethinkDB.NoClash

import GHC.Generics

import Network.URI

import Serials.Model.Lib.Crud
import qualified Serials.Model.Source as Source
import qualified Serials.Model.Chapter as Chapter
import Serials.Model.Source (Source)
import Serials.Model.Scan (Scan)
import qualified Serials.Model.Scan as Scan

import Serials.Types

import System.Environment

scanIntervalSeconds = 10 * 60

data AppInfo = AppInfo {
  appName :: Text,
  appVersion :: Text
} deriving (Eq, Generic)

instance FromJSON AppInfo
instance ToJSON AppInfo

data AppStatus = AppStatus {
  okDatabase :: Bool,
  okScans :: Bool,
  scans :: [ScannedSource]
} deriving (Generic)
instance ToJSON AppStatus

data ScannedSource = ScannedSource {
  sourceId :: Text,
  sourceName :: Text,
  sourceStatus :: Source.Status,
  lastScan :: Maybe Scan
} deriving (Generic)
instance ToJSON ScannedSource

appStatus :: App AppStatus
appStatus = do
  dbh <- checkDbHealth
  scans <- lastScans
  time <- liftIO $ getCurrentTime
  return $ AppStatus dbh (all (isOkScan time) scans) scans

health :: Text -> Bool -> Maybe Text
health msg healthy = if healthy then Just msg else Nothing

------------------------------------------------------

checkDbHealth :: App Bool
checkDbHealth = do
  r <- runDb $ table "sources" # status :: App (Either RethinkDBError Datum)
  return $ True

lastScans :: App [ScannedSource]
lastScans = do
  sources <- Source.list
  time <- liftIO $ getCurrentTime
  let scans = map scannedSource $ filter Source.isActive sources
  return scans

isOkScan :: UTCTime -> ScannedSource -> Bool
isOkScan time (ScannedSource _ _ status mscan) =
  case mscan of
    Nothing -> False
    Just scan -> status /= Source.Active || isRecent time scan

isRecent :: UTCTime -> Scan -> Bool
isRecent time scan =
    (diffUTCTime time (Scan.date scan)) < (2*scanIntervalSeconds)

scannedSource :: Source -> ScannedSource
scannedSource s = ScannedSource (Source.id s) (Source.name s) (Source.status s) (Source.lastScan s)

----------------------------------------------------------------

readAllEnv :: IO Env
readAllEnv = do
    port <- readEnv "PORT" 3001
    environment <- readEnv "ENV" Dev
    endpoint <- defEnv "ENDPOINT" "http://localhost:3001"
    db <- lookupDb
    mm <- lookupEnv "MANDRILL_API_KEY"
    s <- defEnv "AUTH_SECRET" "not a secret"
    case mm of
      Nothing -> error "missing env MANDRILL_API_KEY"
      Just m  -> return $ Env port db (pack m) (pack endpoint) environment (pack s)

lookupDb :: IO (String, Integer)
lookupDb = do
    mdbs <- lookupEnv "RETHINKDB_PORT_28015_TCP"
    let mdb = readEndpoint =<< mdbs
    return $ fromMaybe ("localhost", 28015) mdb

readEnv :: Read a => String -> a -> IO a
readEnv name def = do
    mval <- lookupEnv name
    return $ fromMaybe def (read <$> mval)

defEnv :: String -> String -> IO String
defEnv name def = do
    mval <- lookupEnv name
    return $ fromMaybe def mval

-- tcp://234.234.234.2:28016
readEndpoint :: String -> Maybe (String, Integer)
readEndpoint u = do
    uri <- parseURI u
    auth <- uriAuthority uri
    return $ (uriRegName auth, readPort $ uriPort auth)
  where
    readPort = read . drop 1


-------------------------------------------------------------

