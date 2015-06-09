module Main where

import Data.Char
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Text (pack, Text)

import Control.Applicative

import Serials.Link
import Serials.Api
import Serials.Scan
import Serials.Model.Lib.Crud
import Serials.Model.App

import System.Environment
import Network.URI
import Version


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
    cmd "scan" = mainScan xs
    cmd name   = putStrLn $ "Could not find command: " ++ name
command _ = usage

usage = putStrLn $ "Usage: `serials scan` or `serials api`"

mainApi :: IO ()
mainApi = do
    putStrLn "-- SERIALS API ----------------"
    env <- readAllEnv
    print env
    p <- connectDbPool (db env)
    runApi (port env) p generatedVersion env

mainScan :: [String] -> IO ()
mainScan ids = do
    putStr "[SCAN] | "
    env <- readAllEnv
    p <- connectDbPool (db env)
    case ids of
      [] -> importAllSources p
      is -> mapM_ (importSourceId p) (map pack is)
    return ()

