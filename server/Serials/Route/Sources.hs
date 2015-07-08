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

import Serials.AppMonad
import Serials.Route.App
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

sourcesServerT :: ServerT SourcesAPI App
sourcesServerT =
         sourcesGetAll :<|> sourcesPost
    :<|> sourceScan
    :<|> sourcesGet :<|> sourcesPut
    :<|> changesGet
    :<|> protected hasClaimAdmin (sourcesDel)

  where

  sourcesDel :: Text -> App ()
  sourcesDel id = Source.delete id

  -- do I want to operate in a different monad here?
  -- what would that look like? 
  -- it would be pretty cool, but super fancy...
  sourcesGetAll :: Maybe Text -> App [SourceThumbnail]
  sourcesGetAll mt = map SourceThumbnail <$> getSources
    where
    getSources = case mt of
                  Nothing -> Source.list
                  Just t  -> Source.findByTag t

  sourcesPost :: Maybe Text -> Source -> App Text
  sourcesPost mt s = do
    user <- currentUser mt

    -- save the source
    sourceId <- Source.insert s

    -- save the change, with the new source id attached
    time <- liftIO $ getCurrentTime
    let s' = s { Source.id = sourceId }
        c = change Nothing s' time user
    cid <- Change.insert c

    return sourceId



  sourcesGet :: Text -> App Source
  sourcesGet id = isNotFound $ Source.find id

  sourcesPut :: Text -> Maybe Text -> Source -> App ()
  sourcesPut sourceId mt source = do

    user <- currentUser mt

    old <- isNotFound $ Source.find sourceId

    -- if they are equal just return a 200 status code
    if old == source
    then return ()

    else do
      -- save the change
      time <- liftIO $ getCurrentTime
      let c = change (Source.changeId old) source time user
      cid <- Change.insert c

      -- save the source
      let source' = source { changeId = Just cid }
      isError $ Source.save sourceId source'

  changesGet :: Text -> App [Change]
  changesGet id = Change.findBySourceId id

  sourceScan :: Source -> App ScanResult
  sourceScan source = liftIO $ scanSourceResult source

--sourcesServer :: AppConfig -> Server SourcesAPI
--sourcesServer config = enter (Nat $ (runAppT config)) sourcesServerT

---------------------------------------------------------------

type ChangesAPI =
       Get [Change]
  :<|> Capture "id" Text :> Get Change

changesServerT :: ServerT ChangesAPI App
changesServerT = getAll :<|> getOne

  where

  getAll :: App [Change]
  getAll = Change.list

  getOne :: Text -> App Change
  getOne id = isNotFound $ Change.findById id


--changesServer :: AppConfig -> Server ChangesAPI
--changesServer config = enter (Nat $ (runAppT config)) changesServerT
changesServer = changesServerT
