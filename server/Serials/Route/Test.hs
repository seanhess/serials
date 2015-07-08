{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Serials.Route.Test where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Monoid
import Data.Text (Text, pack)
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Servant.Server
import Servant
import Database.RethinkDB.NoClash
import System.Environment

data AppError = Invalid Text | NotFound | ServerError Text

newtype App a = App {
  runApp :: ReaderT Int (ExceptT AppError IO) a
} deriving (Monad, Functor, Applicative, MonadReader Int, MonadError AppError, MonadIO)

type TestAPI =
         "a" :> Get '[JSON] String
    :<|> "b" :> Get '[JSON] String
    :<|> "c" :> Get '[JSON] String

giveMeAMessage :: App String
giveMeAMessage = do
    code <- ask
    name <- getProgName'
    throwError $ Invalid "Goooo"
    return $ show code <> name

testWoot :: App (Maybe String)
testWoot = return $ Nothing

testErr :: App (Either String String)
testErr = return $ Left "Oh no!"

getProgName' :: MonadIO m => m String
getProgName' = liftIO $ getProgName

woot :: IO String
woot = return "hello"

---------------------------------------------------------------

-- return a 404 if Nothing
isNotFound :: App (Maybe a) -> App a
isNotFound action = do
    res <- action
    case res of
      Nothing -> throwError $ NotFound
      Just v  -> return v

-- map to a generic error
isError :: Show e => App (Either e a) -> App a
isError action = do
    res <- action
    case res of
      Left e -> throwError $ ServerError $ pack $ show e
      Right v -> return v

-- wow, it's IN My monad here! that's swell
testServerT ::ServerT TestAPI App
testServerT = getA :<|> getB :<|> getC
  where

    getA :: App String
    getA = giveMeAMessage
    -- you can also lift IO functions
    --getA = liftIO $ woot

    -- I can map app functions that return Maybes and Eithers to 
    -- app exceptions using little functions like this
    getB :: App String
    getB = isNotFound $ testWoot

    getC :: App String
    getC = isError $ testErr

-- see this is awesome because I can easily map error codes here
-- especially if they are different types?
runAppT :: Int -> App a -> EitherT ServantErr IO a
runAppT code action = do
    res <- liftIO $ runExceptT $ runReaderT (runApp action) code

    -- branch based on the error or value
    EitherT $ return $ case res of
      Left (Invalid text) -> Left err400 { errBody = textToBSL text }
      Left (NotFound)     -> Left err404
      Left (ServerError text) -> Left err500 { errBody = textToBSL text }
      Right a  -> Right a

textToBSL :: Text -> ByteString
textToBSL = encodeUtf8 . fromStrict

testServer' :: Int -> Server TestAPI
testServer' code = enter (Nat $ (runAppT code)) testServerT
