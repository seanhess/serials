{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Serials.Model.App where

import Control.Applicative

import Data.Aeson
import Data.Pool
import Data.Text (Text, pack)
import Data.Maybe

import Database.RethinkDB.NoClash

import GHC.Generics

import Network.URI

import Serials.Model.Lib.Crud

import System.Environment

data AppInfo = AppInfo {
  appName :: Text,
  appVersion :: Text
} deriving (Eq, Generic)

instance FromJSON AppInfo
instance ToJSON AppInfo

data AppStatus = AppStatus {
  health :: Text
} deriving (Generic)
instance ToJSON AppStatus

appStatus :: Pool RethinkDBHandle -> IO AppStatus
appStatus h = do
  runPool h $ table "sources" # status :: IO (Either RethinkDBError Datum)
  return $ AppStatus "OK"

--------------------------------------------------------

data Env = Env {
  port :: Int,
  db :: (String, Integer),
  mandrill :: Text,
  endpoint :: Text
} deriving (Show)

readAllEnv :: IO Env
readAllEnv = do
    port <- readEnv "PORT" 3001
    endpoint <- defEnv "ENDPOINT" "http://localhost:3001"
    db <- lookupDb
    mm <- lookupEnv "MANDRILL_API_KEY"
    case mm of
      Nothing -> error "missing env MANDRILL_API_KEY"
      Just m  -> return $ Env port db (pack m) (pack endpoint)

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

