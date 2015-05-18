{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Serials.Route.UserSignup where

import Prelude hiding (id)

import Control.Applicative
import Crypto.BCrypt (hashPasswordUsingPolicy, HashingPolicy(..))

import Data.Text (Text, pack, unpack, toLower)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Aeson (ToJSON, FromJSON)
import Data.Time
import Data.Pool
import Data.Maybe (isJust, isNothing)

import GHC.Generics
import Database.RethinkDB.NoClash hiding (table)

import qualified Serials.Model.User as User
import Serials.Model.User (User)
import qualified Serials.Model.Invite as Invite

data UserSignup = UserSignup {
  firstName :: Text,
  lastName :: Text,
  email :: Text,
  code :: Text,
  password :: Text,
  passwordConfirmation :: Text
} deriving (Show, Generic)

instance FromJSON UserSignup
instance ToJSON UserSignup
instance FromDatum UserSignup
instance ToDatum UserSignup

signup :: Pool RethinkDBHandle -> UserSignup -> IO (Either Text User)
signup h u = do
  if (password u) /= (passwordConfirmation u)
    then return $ Left "Password and Password Confirmation do not match"
  else do
    existUser  <- User.findByEmail h $ toLower $ email u
    mInvite <- Invite.find h (code u)

    if isJust existUser
      then return $ Left "User already exists with that email"
    else case mInvite of
      Nothing -> return $ Left "Invite not found"
      Just invite ->
        if isJust (Invite.userId invite)
          then return $ Left "Invite already used"
        else do
          mu <- newUser u
          case mu of
            Nothing -> return $ Left "Could not hash user password"
            Just user -> do
              createdUser <- User.insert h user
              Invite.markUsed h (code u) (User.id createdUser)
              return $ Right createdUser

newUser :: UserSignup -> IO (Maybe User)
newUser u = do
  created <- getCurrentTime
  hashPass <- hashPasswordUsingPolicy customHashPolicy $ encodeUtf8 (password u)

  case hashPass of
    Nothing -> return Nothing
    Just pass -> return $ Just $ User.User {
      User.id             = pack "",
      User.firstName      = firstName u,
      User.lastName       = lastName u,
      User.email          = toLower $ email u,
      User.hashedPassword = Just $ decodeUtf8 pass,
      User.admin          = False,
      User.created        = created
    }

customHashPolicy :: HashingPolicy
customHashPolicy = HashingPolicy 10 "$2b$"

