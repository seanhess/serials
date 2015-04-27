module Main where

import Data.Char
import Data.Maybe (fromMaybe)
import Data.Monoid

import Control.Applicative

import Serials.Link
import Serials.Api

import System.Environment
import Network.URI


-- dispatches multiple possible programs to run based on the name
main :: IO ()
main = do
  args <- getArgs
  let cmd = command args
  cmd
  return ()

command :: [String] -> IO ()
command (name:xs) = cmd name
  where
    cmd "api"  = mainApi
    cmd "scan" = mainScan
    cmd name   = putStrLn $ "Could not find command: " ++ name
command _ = usage

usage = putStrLn $ "Usage: `serials scan` or `serials api`"

mainApi :: IO ()
mainApi = do
    putStrLn "-- SERIALS API ----------------"
    port <- readEnv "PORT" 3001
    putStrLn $ "PORT: " <> show port
    db <- lookupDb
    putStrLn $ "DB: " <> show db
    runApi port db

mainScan :: IO ()
mainScan = do
    putStrLn "-- SERIALS SCAN --------------"
    db <- lookupDb
    putStrLn $ "DB: " <> show db
    putStrLn "SCAN NOW"


lookupDb :: IO (String, Integer)
lookupDb = do
    mdbs <- lookupEnv "RETHINKDB_PORT_28015_TCP"
    let mdb = readEndpoint =<< mdbs
    return $ fromMaybe ("localhost", 28015) mdb

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
