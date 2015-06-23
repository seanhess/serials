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

import Database.RethinkDB.NoClash

import Serials.Route.Route
import Serials.Route.Auth (AuthToken, checkAuth, currentUser)

import Serials.Model.Source (Source(..), Change(..), ChangeKind(..))
import Serials.Model.Chapter (Chapter(..))
import qualified Serials.Model.Source as Source
import qualified Serials.Model.Chapter as Chapter
import Serials.Scan (importSourceId)

import Servant hiding (Get, Post, Put, Delete, ReqBody)

type SourcesAPI =
       Get [Source]
  :<|> AuthToken :> ReqBody Source :> Post Text

   :<|> Capture "id" Text :> Get Source
   :<|> Capture "id" Text :> AuthToken :> ReqBody Source :> Put ()

   :<|> Capture "id" Text :> "chapters" :> Get [Chapter]
   :<|> Capture "id" Text :> "chapters" :> Post ()
   :<|> Capture "id" Text :> "chapters" :> Delete ()

sourcesServer :: Pool RethinkDBHandle -> Server SourcesAPI
sourcesServer h =
        sourcesGetAll :<|> sourcesPost
    :<|> sourcesGet :<|> sourcesPut
    :<|> chaptersGet :<|> sourceScan :<|> chaptersDel

  where

  sourcesGetAll :: Handler [Source]
  sourcesGetAll = liftIO $ Source.list h

  sourcesPost :: Maybe Text -> Source -> Handler Text
  sourcesPost mt s = do
    user <- currentUser h mt
    liftIO $ do
      Source.saveChange h =<< Source.change Create user s
      Source.insert h s

  sourcesGet :: Text -> Handler Source
  sourcesGet id   = liftE  $ Source.find h id

  sourcesPut :: Text -> Maybe Text -> Source -> Handler ()
  sourcesPut id mt s = do
    user <- currentUser h mt
    liftIO $ do
      Source.saveChange h =<< Source.change Create user s
      Source.save h id s

  chaptersGet :: Text -> Handler [Chapter]
  chaptersGet id = liftIO $ Chapter.bySource h id

  chaptersDel :: Text -> Handler ()
  chaptersDel id = liftIO $ Chapter.deleteBySource h id

  sourceScan :: Text -> Handler ()
  sourceScan  id = liftIO $ importSourceId h id

