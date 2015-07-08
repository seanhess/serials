{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Serials.AppMonad where

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
  runApp :: ReaderT AppConfig (ExceptT ServantErr IO) a
} deriving (Monad, Functor, Applicative, MonadReader AppConfig, MonadError ServantErr, MonadIO)

runAppT :: AppConfig -> App a -> EitherT ServantErr IO a
runAppT config action = do
    res <- liftIO $ runExceptT $ runReaderT (runApp action) config
    EitherT $ return $ case res of
      Left err -> Left err
      Right v  -> Right v

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


-------------------------------------------------------------

--type TestAPI =
         --"a" :> Get '[JSON] String
    -- :<|> "b" :> Get '[JSON] String
    -- :<|> "c" :> Get '[JSON] String

--giveMeAMessage :: App String
--giveMeAMessage = do
    --code <- ask
    --name <- getProgName'
    --return $ show code <> name

--testWoot :: App (Maybe String)
--testWoot = return $ Nothing

--testErr :: App (Either String String)
--testErr = return $ Left "Oh no!"

--getProgName' :: MonadIO m => m String
--getProgName' = return "hello"

--woot :: IO String
--woot = return "hello"

--isNotFound :: App (Maybe a) -> App a
--isNotFound action = do
    --res <- action
    --case res of
      --Nothing -> throwError $ err404
      --Just v  -> return v

---- map to a generic error
--isError :: Show e => App (Either e a) -> App a
--isError action = do
    --res <- action
    --case res of
      --Left e -> throwError $ err500 { errBody = BL.pack $ show e }
      --Right v -> return v

--isInvalidText :: App (Either Text a) -> App a
--isInvalidText action = do
    --res <- action
    --case res of
      --Left e -> throwError $ err400 { errBody = TLE.encodeUtf8 $ TL.fromStrict e }
      --Right v -> return v


---- wow, it's IN My monad here! that's swell
--testServerT ::ServerT TestAPI App
--testServerT = getA :<|> getB :<|> getC
  --where

    --getA :: App String
    --getA = giveMeAMessage
    ---- you can also lift IO functions
    ----getA = liftIO $ woot

    ---- I can map app functions that return Maybes and Eithers to 
    ---- app exceptions using little functions like this
    --getB :: App String
    --getB = isNotFound $ testWoot

    --getC :: App String
    --getC = isError $ testErr



--testServer' :: AppConfig -> Server TestAPI
--testServer' config = enter (Nat $ (runAppT config)) testServerT
