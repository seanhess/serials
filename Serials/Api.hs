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

import Data.Aeson (ToJSON)
import Data.Monoid
import Data.Proxy
import Data.Text (Text, toUpper)
import Data.Maybe (fromJust)

import GHC.Generics

import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp (run, Port)
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.AddHeaders

import Serials.Model.Source
import Serials.Model.Chapter 
import Serials.Scan

import Web.Scotty

import Database.RethinkDB.NoClash (RethinkDBHandle, use, db, connect)


routes :: RethinkDBHandle -> ScottyM ()
routes h = do
  
  middleware $ cors (const $ Just corsResourcePolicy)

  get "/" $ text "Serials"

  get "/sources" $ do
    ss <- liftIO $ sourcesList h
    json ss
  
  post "/sources" $ do
    source <- jsonData :: ActionM Source
    result <- liftIO $ sourcesCreate h source
    json source

  get "/sources/:id" $ do
    id <- param "id"
    ms <- liftIO $ sourcesFind h id
    maybeJson ms

  delete "/sources/:id" $ do
    id <- param "id"
    liftIO $ sourcesRemove h id    
    text "OK"

  put "/sources/:id" $ do
    id <- param "id"
    s <- jsonData
    liftIO $ sourcesSave h id s
    text "OK"

  get "/sources/:id/chapters" $ do
    id <- param "id"
    result <- liftIO $ chaptersBySource h id
    json result

  -- scan!
  post "/sources/:id/chapters" $ do
    id <- param "id"
    result <- liftIO $ importSource h id
    liftIO $ print result
    text "OK"

-- Run ---------------------------------------------------------

runApi :: Int -> IO ()
runApi port = do
    putStrLn $ "Running on " <> show port
    h <- connectDb
    sourcesInit h
    chaptersInit h
    scotty port (routes h)
    return ()

-- DB -----------------------------------------------------------

connectDb :: IO RethinkDBHandle
connectDb = use serialsDb <$> connect "localhost" 28015 Nothing

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

--------------------------------------------------------------------

--  move to library 
maybeJson :: (ToJSON a) => Maybe a -> ActionM ()
maybeJson (Just a) = json a
maybeJson Nothing = do
    status status404
    text "Not Found"
