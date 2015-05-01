{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Serials.Lib.Auth where

import Data.Text (Text, unpack)
import Data.ByteString.Char8 (pack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.ByteString hiding (head, last, pack, unpack)
import Data.Maybe (listToMaybe, fromJust)
import Data.Pool (Pool)
import Database.RethinkDB.NoClash (RethinkDBHandle)
import Crypto.BCrypt (validatePassword)
import Network.HTTP.Types
import Network.Wai
import Servant
import Servant.Server.Internal
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import qualified Data.Text as T

import Serials.Model.User (User (hashedPassword))
import qualified Serials.Model.User as User hiding (User())

data UserLogin = UserLogin {
  email :: Text
  , password :: Text
} deriving (Show, Generic)

instance FromJSON UserLogin
instance ToJSON UserLogin

type TokenLookup = ByteString -> IO Bool

data WithAuthToken

instance HasServer api => HasServer (WithAuthToken :> api) where
    type Server (WithAuthToken :> api) = (TokenLookup, Server api)

    route Proxy (tokenLookup, serv) request respond = case lookup "Authorization" (requestHeaders request) of
      Nothing -> respond . succeedWith $ responseLBS status401 [] ""
      Just x  -> do
        let auth = getAuthToken x
        cookie <- tokenLookup auth
        if cookie
          then route (Proxy :: Proxy api) serv request respond
          else respond . succeedWith $ responseLBS status403 [] ""

getAuthToken :: ByteString -> ByteString
getAuthToken token = encodeUtf8 . last $ T.split (==' ') $ decodeUtf8 token

checkCurrentAuth :: Pool RethinkDBHandle -> Maybe Text -> IO (Maybe User)
checkCurrentAuth h token = case token of
  Nothing -> return Nothing
  Just t -> do
    users <- User.findByToken h t
    case listToMaybe users of
      Nothing -> return Nothing
      Just x -> return $ Just x

checkAuthToken :: Pool RethinkDBHandle -> TokenLookup
checkAuthToken h token = do
  users <- User.findByToken h $ decodeUtf8 $ token
  case listToMaybe users of
    Nothing -> return False
    Just x -> return True

-- Helpers should we move these to their respective models or to Helpers.hs or somewhere else?
textToByteString :: Text -> ByteString
textToByteString = pack . unpack

userPassword :: UserLogin -> Text
userPassword = password

userHashedPassword :: User -> Text
userHashedPassword = fromJust . User.hashedPassword
--

userLogin :: Pool RethinkDBHandle -> UserLogin -> IO (Either Text User)
userLogin h u = do
  users <- User.findByEmail h $ email u
  case listToMaybe users of
    Nothing -> return $ Left "Invalid email address"
    Just user -> do
      let hashedPassword = textToByteString $ userHashedPassword user
      let password = textToByteString $ userPassword u
      case validatePassword hashedPassword password of
        False -> return $ Left "Invalid password"
        True -> return $ Right user

