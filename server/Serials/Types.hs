{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Serials.Types where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Trans.Either
import Control.Monad.IO.Class (liftIO)

import Data.Aeson (ToJSON)
import Data.Pool (Pool)
import Data.Monoid
import Data.Text (Text, unpack)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL
import qualified Database.RethinkDB as RethinkDB
import Database.RethinkDB (RethinkDBHandle)

import GHC.Generics

import Servant

data AppConfig = AppConfig {
  appName :: Text,
  version :: Text,
  env :: Env,
  conn :: Pool RethinkDBHandle
} deriving (Show)

newtype App a = App {
  unApp :: ReaderT AppConfig (ExceptT ServantErr IO) a
} deriving (Monad, Functor, Applicative, MonadReader AppConfig, MonadError ServantErr, MonadIO)

runApp :: AppConfig -> App a -> EitherT ServantErr IO a
runApp config action = do
    res <- liftIO $ runExceptT $ runReaderT (unApp action) config
    EitherT $ return res


--------------------------------------------------------

type Endpoint = Text

data AppEnvironment = Dev | Production deriving (Show, Generic, Read)
instance ToJSON AppEnvironment

data Env = Env {
  port :: Int,
  envDb :: (String, Integer),
  mandrill :: Text,
  endpoint :: Endpoint,
  environment :: AppEnvironment,
  authSecret :: Text
} deriving (Show)

askEndpoint :: App Endpoint
askEndpoint = asks (endpoint . env)

