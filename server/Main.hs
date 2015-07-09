{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Text (pack, Text)

import Control.Monad.IO.Class (liftIO)
import Control.Applicative
import Control.Monad.Trans.Either

import Serials.Link
import Serials.Api
import Serials.Scan
import Serials.Model.Lib.Crud
import Serials.Model.App
import Serials.Types

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
    config <- initConfig
    runApi config

mainScan :: [String] -> IO ()
mainScan ids = do
    putStr "[SCAN] | "
    config <- initConfig
    res <- runEitherT $ runApp config $ do
      case ids of
        [] -> importAllSources
        is -> mapM_ (importSourceId) (map pack is)
    return ()

initConfig :: IO AppConfig
initConfig = do
  env <- readAllEnv
  print env
  p <- connectDbPool (envDb env)
  return $ AppConfig "serials" (pack generatedVersion) env p
