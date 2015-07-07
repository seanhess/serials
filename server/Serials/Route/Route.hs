{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Serials.Route.Route where

import Control.Applicative
import Control.Monad.Reader (runReaderT)
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


import Serials.Model.Lib.Crud (RethinkIO)

import Servant hiding (Get, Post, Put, Delete, ReqBody)
import qualified Servant

-- if you use (Maybe a) with liftIO it will return null instead of a 404
-- this is intentional for some routes

-- we only read / write JSON
type Get    a  = Servant.Get    '[JSON] a
type Post   a  = Servant.Post   '[JSON] a
type Put    a  = Servant.Put    '[JSON] a
type Delete a  = Servant.Delete '[JSON] a
type ReqBody a = Servant.ReqBody '[JSON] a

type Handler a = EitherT ServantErr IO a

-- ToStatus ------------------------------------------------------

class ToStatus a where
  toStatus :: a val -> Either ServantErr val

instance ToStatus Maybe where
  toStatus Nothing  = Left $ err404
  toStatus (Just v) = Right v

instance Show a => ToStatus (Either a) where
  toStatus (Left e) = Left $ err500 { errBody = "Server Error: " <> BL.pack (show e) }
  toStatus (Right v) = Right v

liftE :: ToStatus a => IO (a v) -> EitherT ServantErr IO v
liftE action = EitherT $ toStatus <$> action

errUnauthorized :: Text -> ServantErr
errUnauthorized err = err401 { errReasonPhrase = unpack err }

errText :: ServantErr -> Text -> ServantErr
errText err txt = err { errBody = TLE.encodeUtf8 $ TL.fromStrict txt }

liftErr :: (err -> ServantErr) -> IO (Either err res) -> EitherT ServantErr IO res
liftErr toErr action = do
    eres <- liftIO action
    case eres of
      Left err -> left (toErr err)
      Right res -> return res

liftErrText :: ServantErr -> IO (Either Text res) -> EitherT ServantErr IO res
liftErrText err = liftErr (errText err)

liftDb :: Pool RethinkDBHandle -> RethinkIO a -> Handler a
liftDb h a = liftIO $ runReaderT a h

