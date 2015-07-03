{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Serials.Route.Sources where

import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either

import Data.Aeson (ToJSON)
import Data.Time
import Data.Text (Text)
import Data.Pool (Pool)
import Data.Maybe (fromMaybe)

import Database.RethinkDB.NoClash hiding (Change)

import GHC.Generics

import Serials.Route.Route
import Serials.Route.Auth (AuthToken, checkAuth, currentUser, AuthProtected, protected, hasClaimAdmin)

import Serials.Model.Source (Source(..), SourceThumbnail(..))
import Serials.Model.Change (Change(..), change)
import Serials.Model.Scan (Scan(..))
import Serials.Model.Chapter (Chapter(..))
import qualified Serials.Model.Source as Source
import qualified Serials.Model.Chapter as Chapter
import qualified Serials.Model.Change as Change
import Serials.Scan (importSourceId, scanSourceResult, ScanResult, allChapters)

import Servant hiding (Get, Post, Put, Delete, ReqBody)

type SourcesAPI =
        QueryParam "tag" Text :> Get [SourceThumbnail]
   :<|> AuthToken :> ReqBody Source :> Post Text

   :<|> "scan" :> ReqBody Source :> Post ScanResult

   :<|> Capture "id" Text :> Get Source
   :<|> Capture "id" Text :> AuthToken :> ReqBody Source :> Put ()

   :<|> Capture "id" Text :> "changes" :> Get [Change]

   :<|> AuthProtected :> SourcesAdminAPI

type SourcesAdminAPI = Capture "id" Text :> Delete ()

sourcesServer :: Pool RethinkDBHandle -> Server SourcesAPI
sourcesServer h =
         sourcesGetAll :<|> sourcesPost
    :<|> sourceScan
    :<|> sourcesGet :<|> sourcesPut
    :<|> changesGet
    :<|> protected hasClaimAdmin (sourcesDel)

  where

  sourcesDel :: Text -> Handler ()
  sourcesDel id   = liftIO $ Source.delete h id

  sourcesGetAll :: Maybe Text -> Handler [SourceThumbnail]
  sourcesGetAll mt = liftIO $ map SourceThumbnail <$> getSources
    where
    getSources = case mt of
                  Nothing -> Source.list h
                  Just t  -> Source.findByTag h t

  sourcesPost :: Maybe Text -> Source -> Handler Text
  sourcesPost mt s = do
    user <- currentUser h mt

    -- save the source
    sourceId <- liftIO $ Source.insert h s

    -- save the change, with the new source id attached
    time <- liftIO $ getCurrentTime
    let s' = s { Source.id = sourceId }
        c = change Nothing s' time user
    cid <- liftIO $ Change.insert h c

    return sourceId



  sourcesGet :: Text -> Handler Source
  sourcesGet id   = liftE  $ Source.find h id

  sourcesPut :: Text -> Maybe Text -> Source -> Handler ()
  sourcesPut sourceId mt source = do

    user <- currentUser h mt

    old <- liftE $ Source.find h sourceId

    -- if they are equal just return a 200 status code
    if old == source
    then return ()

    else do
      -- save the change
      time <- liftIO $ getCurrentTime
      let c = change (Source.changeId old) source time user
      cid <- liftIO $ Change.insert h c

      -- save the source
      let source' = source { changeId = Just cid }
      liftE $ Source.save h sourceId source'

  changesGet :: Text -> Handler [Change]
  changesGet id = liftIO $ Change.findBySourceId h id

  sourceScan :: Source -> Handler ScanResult
  sourceScan source = liftIO $ scanSourceResult source


---------------------------------------------------------------

type ChangesAPI =
       Get [Change]
  :<|> Capture "id" Text :> Get Change

changesServer :: Pool RethinkDBHandle -> Server ChangesAPI
changesServer h = getAll :<|> getOne

  where

  getAll :: Handler [Change]
  getAll = liftIO $ Change.list h

  getOne :: Text -> Handler Change
  getOne id = liftE $ Change.findById h id
