module Main where

import Data.Char
import Data.Maybe (fromMaybe)
import Data.Monoid

import Control.Applicative

import Serials.Link
import Serials.Api

import System.Environment
import Network.URI

main = do
    putStrLn "-- SERIALS -------------------"
    port <- readEnv "PORT" 3001
    mdbs <- lookupEnv "RETHINKDB_PORT_28015_TCP"
    let mdb = readEndpoint =<< mdbs
        db = fromMaybe ("localhost", 28015) mdb
    putStrLn $ "PORT: " <> show port
    putStrLn $ "DB: " <> show db
    runApi port db

readEnv :: Read a => String -> a -> IO a
readEnv name def = do
    mval <- lookupEnv name
    return $ fromMaybe def (read <$> mval)

-- tcp://234.234.234.2:28016
readEndpoint :: String -> Maybe (String, Integer)
readEndpoint u = do
    uri <- parseURI u
    auth <- uriAuthority uri
    return $ (uriRegName auth, readPort $ uriPort auth)
  where
    readPort = read . drop 1

