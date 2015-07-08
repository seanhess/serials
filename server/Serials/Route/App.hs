{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Serials.Route.App where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Trans.Either
import Control.Monad.IO.Class (liftIO)

import Data.Pool (Pool)
import Data.Monoid
import Data.Text (Text, unpack)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL
import qualified Database.RethinkDB as RethinkDB
import Database.RethinkDB (RethinkDBHandle)


import Serials.AppMonad

import Servant hiding (Get, Post, Put, Delete, ReqBody)
import qualified Servant

-- if you use (Maybe a) with liftIO it will return null instead of a 404
-- this is intentional for some routes

-- we only read / write JSON
type Get    a  = Servant.Get     '[JSON] a
type Post   a  = Servant.Post    '[JSON] a
type Put    a  = Servant.Put     '[JSON] a
type Delete a  = Servant.Delete  '[JSON] a
type ReqBody a = Servant.ReqBody '[JSON] a



---------------------------------------------------------------------

isNotFound :: App (Maybe a) -> App a
isNotFound action = do
    res <- action
    case res of
      Nothing -> throwError $ err404
      Just v  -> return v

-- map to a generic error
isError :: Show e => App (Either e a) -> App a
isError action = do
    res <- action
    case res of
      Left e -> throwError $ err500 { errBody = BL.pack $ show e }
      Right v -> return v

isInvalidText :: App (Either Text a) -> App a
isInvalidText action = do
    res <- action
    case res of
      Left e -> throwError $ err400 { errBody = TLE.encodeUtf8 $ TL.fromStrict e }
      Right v -> return v

-- what if it could return an invalid error OR an application error?
-- well, it already handles fail, just have it throw if needed
-- it might be nice to map other types, like .... using ExceptT for invalid stuff

----------------------------------------------------------------

--type TestAPI =
         --"a" :> Get String
    -- :<|> "b" :> Get String
    -- :<|> "c" :> Get String

--testServer' :: AppConfig -> Server TestAPI
--testServer' config = enter (Nat $ (runAppT config)) testServerT

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


--giveMeAMessage :: App String
--giveMeAMessage = do
    --one <- asks appName
    --two <- return "asdf"
    ----throwError $ Invalid "Goooo"
    --return $ show one <> two

--testWoot :: App (Maybe String)
--testWoot = return $ Nothing

--testErr :: App (Either String String)
--testErr = return $ Left "Oh no!"

----getProgName' :: MonadIO m => m String
----getProgName' = liftIO $ getProgName

--woot :: IO String
--woot = return "hello"

-- ToStatus ------------------------------------------------------

--class ToStatus a where
  --toStatus :: a val -> Either ServantErr val

--instance ToStatus Maybe where
  --toStatus Nothing  = Left $ err404
  --toStatus (Just v) = Right v

--instance Show a => ToStatus (Either a) where
  --toStatus (Left e) = Left $ err500 { errBody = "Server Error: " <> BL.pack (show e) }
  --toStatus (Right v) = Right v

--liftE :: ToStatus a => IO (a v) -> EitherT ServantErr IO v
--liftE action = EitherT $ toStatus <$> action

--errUnauthorized :: Text -> ServantErr
--errUnauthorized err = err401 { errReasonPhrase = unpack err }

--errText :: ServantErr -> Text -> ServantErr
--errText err txt = err { errBody = TLE.encodeUtf8 $ TL.fromStrict txt }

--liftErr :: (err -> ServantErr) -> IO (Either err res) -> EitherT ServantErr IO res
--liftErr toErr action = do
    --eres <- liftIO action
    --case eres of
      --Left err -> left (toErr err)
      --Right res -> return res

--liftErrText :: ServantErr -> IO (Either Text res) -> EitherT ServantErr IO res
--liftErrText err = liftErr (errText err)

--liftDb :: Pool RethinkDBHandle -> RethinkIO a -> Handler a
--liftDb h a = liftIO $ runReaderT a h

