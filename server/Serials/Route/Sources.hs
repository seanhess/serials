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

import Data.Time
import Data.Text (Text)
import Data.Pool (Pool)
import Data.Maybe (fromMaybe)

import Database.RethinkDB.NoClash hiding (Change)

import Serials.Route.Route
import Serials.Route.Auth (AuthToken, checkAuth, currentUser, AuthProtected, protected, hasClaimAdmin)

import Serials.Model.Source (Source(..), SourceThumbnail(..))
import Serials.Model.Change (Change(..), change)
import Serials.Model.Chapter (Chapter(..))
import qualified Serials.Model.Source as Source
import qualified Serials.Model.Chapter as Chapter
import qualified Serials.Model.Change as Change
import Serials.Scan (importSourceId, scanSourceChapters)

import Servant hiding (Get, Post, Put, Delete, ReqBody)

type SourcesAPI =
       Get [SourceThumbnail]
   :<|> AuthToken :> ReqBody Source :> Post Text

   :<|> "scan" :> ReqBody Source :> Post [Chapter]

   :<|> Capture "id" Text :> Get Source
   :<|> Capture "id" Text :> AuthToken :> ReqBody Source :> Put ()

   :<|> Capture "id" Text :> "changes" :> Get [Change]

   :<|> Capture "id" Text :> "chapters" :> Get [Chapter]

   :<|> AuthProtected :> SourcesAdminAPI

type SourcesAdminAPI = Capture "id" Text :> Delete ()

sourcesServer :: Pool RethinkDBHandle -> Server SourcesAPI
sourcesServer h =
         sourcesGetAll :<|> sourcesPost
    :<|> sourceScan
    :<|> sourcesGet :<|> sourcesPut
    :<|> changesGet
    :<|> chaptersGet
    :<|> protected hasClaimAdmin (sourcesDel)

  where

  sourcesDel :: Text -> Handler ()
  sourcesDel id   = liftIO $ Source.delete h id

  -- i need to serialize them WITHOUT all the fancy fields
  sourcesGetAll :: Handler [SourceThumbnail]
  sourcesGetAll = liftIO $ map SourceThumbnail <$> Source.list h

  sourcesPost :: Maybe Text -> Source -> Handler Text
  sourcesPost mt s = do
    user <- currentUser h mt

    -- save the change
    time <- liftIO $ getCurrentTime
    let c = change Nothing s time user
    cid <- liftIO $ Change.insert h c

    -- save the source
    liftIO $ Source.insert h s

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

  chaptersGet :: Text -> Handler [Chapter]
  chaptersGet id = do
    source <- liftE $ Source.find h id
    liftIO $ scanSourceChapters source

  sourceScan :: Source -> Handler [Chapter]
  sourceScan source = liftIO $ scanSourceChapters source


---------------------------------------------------------------

type ChangesAPI = Capture "id" Text :> Get Change

changesServer :: Pool RethinkDBHandle -> Server ChangesAPI
changesServer h = changeGet

  where

  changeGet :: Text -> Handler Change
  changeGet id = liftE $ Change.findById h id
