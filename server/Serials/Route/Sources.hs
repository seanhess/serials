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

import Data.Text (Text)
import Data.Pool (Pool)
import Data.Maybe (fromMaybe)

import Database.RethinkDB.NoClash hiding (Change)

import Serials.Route.Route
import Serials.Route.Auth (AuthToken, checkAuth, currentUser)

import Serials.Model.Source (Source(..), SourceThumbnail(..))
import Serials.Model.Change (Change(..), ChangeKind(..), change)
import Serials.Model.Chapter (Chapter(..))
import qualified Serials.Model.Source as Source
import qualified Serials.Model.Chapter as Chapter
import qualified Serials.Model.Change as Change
import Serials.Scan (importSourceId)

import Servant hiding (Get, Post, Put, Delete, ReqBody)

type SourcesAPI =
       Get [SourceThumbnail]
   :<|> AuthToken :> ReqBody Source :> Post Text

   :<|> Capture "id" Text :> Get Source
   :<|> Capture "id" Text :> AuthToken :> ReqBody Source :> Put ()

   :<|> Capture "id" Text :> "changes" :> Get [Change]

   :<|> Capture "id" Text :> "changes" :> Capture "changeId" Text :> Get Change

   :<|> Capture "id" Text :> "chapters" :> Get [Chapter]
   :<|> Capture "id" Text :> "chapters" :> Post ()
   :<|> Capture "id" Text :> "chapters" :> Delete ()

sourcesServer :: Pool RethinkDBHandle -> Server SourcesAPI
sourcesServer h =
        sourcesGetAll :<|> sourcesPost
    :<|> sourcesGet :<|> sourcesPut
    :<|> changesGet :<|> changeGet
    :<|> chaptersGet :<|> sourceScan :<|> chaptersDel

  where

  -- i need to serialize them WITHOUT all the fancy fields
  sourcesGetAll :: Handler [SourceThumbnail]
  sourcesGetAll = liftIO $ map SourceThumbnail <$> Source.list h

  sourcesPost :: Maybe Text -> Source -> Handler Text
  sourcesPost mt s = do
    user <- currentUser h mt
    liftIO $ do
      --Change.save h =<< change Create user s
      Source.insert h s

  sourcesGet :: Text -> Handler Source
  sourcesGet id   = liftE  $ Source.find h id

  sourcesPut :: Text -> Maybe Text -> Source -> Handler ()
  sourcesPut id mt s = do
    user <- currentUser h mt
    liftE $ do
      --Change.save h =<< change Edit user s
      Source.save h id s

  changesGet :: Text -> Handler [Change]
  changesGet id = return [] -- liftIO $ Change.findBySourceId h id

  changeGet :: Text -> Text -> Handler Change
  changeGet id changeId = liftE $ Change.findById h changeId

  chaptersGet :: Text -> Handler [Chapter]
  chaptersGet id = liftIO $ do
    source <- Source.find h id
    return $ fromMaybe [] $ Source.chapters <$> source

  chaptersDel :: Text -> Handler ()
  chaptersDel id = liftIO $ Source.deleteChapters h id

  sourceScan :: Text -> Handler ()
  sourceScan  id = liftIO $ importSourceId h id

