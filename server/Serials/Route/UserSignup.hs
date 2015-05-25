{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Serials.Route.UserSignup where

import Prelude hiding (id)

import Control.Monad.IO.Class (liftIO)
import Control.Applicative
import Crypto.BCrypt (hashPasswordUsingPolicy, HashingPolicy(..))
import qualified Crypto.BCrypt as Crypto

import Data.Text (Text, pack, unpack, toLower)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Aeson (ToJSON, FromJSON)
import Data.Time
import Data.Pool
import Data.Maybe (isJust, isNothing, fromJust)
import Data.Either (lefts)

import GHC.Generics
import Database.RethinkDB.NoClash hiding (table)

import qualified Serials.Model.User as User
import Serials.Model.User (User)
import qualified Serials.Model.Invite as Invite
import Serials.Lib.Mail
import Serials.Model.Invite (Invite)

data UserSignup = UserSignup {
  firstName :: Text,
  lastName :: Text,
  email :: Text,
  code :: Maybe Text,
  currentPassword :: Maybe Text,
  password :: Maybe Text,
  passwordConfirmation :: Maybe Text
} deriving (Show, Generic)

instance FromJSON UserSignup
instance ToJSON UserSignup
instance FromDatum UserSignup
instance ToDatum UserSignup

signup :: Pool RethinkDBHandle -> UserSignup -> IO (Either Text User)
signup h u = validate (validateSignup h u) $ do
  mu <- newUser u
  case mu of
    Nothing -> return $ Left "Could not hash user password"
    Just user -> do
      createdUser <- User.insert h user
      Invite.markUsed h (fromJust $ code u) (User.id createdUser)
      sendWelcomeEmail createdUser
      return $ Right createdUser

update :: Pool RethinkDBHandle -> Text -> UserSignup -> IO (Either Text User)
update h i u = do
  mu <- User.find h i
  case currentPassword u of
    Nothing -> return $ Left "Current password required to make changes"
    Just cPass -> case mu of
      Nothing -> return $ Left "Not Found"
      Just user -> do
        let hashPass = encodeUtf8 . fromJust $ User.hashedPassword user
        let pass = encodeUtf8 cPass
        case Crypto.validatePassword hashPass pass of
          False -> return $ Left "Invalid password"
          True -> do
            let usr = updateUser user u
            updatedUser <- User.replace h i usr
            return $ Right updatedUser

updateUser :: User -> UserSignup -> User
updateUser u s = u {
  User.firstName = firstName s
  , User.lastName = lastName s
  , User.email = email s
  }

newUser :: UserSignup -> IO (Maybe User)
newUser u = do
  created <- getCurrentTime
  hashPass <- hashPasswordUsingPolicy customHashPolicy $ encodeUtf8 (fromJust $ password u)

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






-- Validation --------------------------------------------------
-- maybe there's a library function for this
validate :: IO (Either Text ()) -> IO (Either Text a) -> IO (Either Text a)
validate validator rest = do
  result <- validator
  case result of
    Left err -> return $ Left err
    Right _  -> rest

validatePassword :: UserSignup -> Either Text ()
validatePassword u = if (password u) /= (passwordConfirmation u)
    then Left "Password and Password Confirmation do not match"
    else Right ()

validateEmail :: Pool RethinkDBHandle -> UserSignup -> IO (Either Text ())
validateEmail h u = do
  existUser <- User.findByEmail h $ toLower $ email u
  return $ if isJust existUser
    then Left "User already exists with that email"
    else Right ()

validateInvite :: Pool RethinkDBHandle -> UserSignup -> IO (Either Text ())
validateInvite h u = do
  mInvite <- Invite.find h (fromJust $ code u)
  case mInvite of
    Nothing -> return $ Left "Invite not found"
    Just invite ->
      return $ if isJust (Invite.userId invite)
        then Left "Invite already used"
        else Right ()

validateSignup :: Pool RethinkDBHandle -> UserSignup -> IO (Either Text ())
validateSignup h u = do
  errs <- lefts <$> sequence validators
  return $ case errs of
    []   -> Right ()
    x:xs -> Left x

  where
  validators = [ return $ validatePassword u
               , validateEmail h u
               , validateInvite h u ]
