{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Serials.Route.UserSignup where

import Prelude hiding (id)

import Control.Applicative
import Control.Monad.Trans.Either
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Crypto.BCrypt (hashPasswordUsingPolicy, HashingPolicy(..))

import Data.Text (Text, pack, unpack, toLower)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Aeson (ToJSON, FromJSON)
import Data.Time
import Data.Pool
import Data.Maybe (isJust, isNothing)
import Data.Either (lefts)
import Data.Monoid ((<>))

import GHC.Generics
import Database.RethinkDB.NoClash (RethinkDBHandle, FromDatum(..), ToDatum(..))

import Serials.Route.App

import qualified Serials.Model.User as User
import Serials.Model.User (User)
import qualified Serials.Model.Invite as Invite
import Serials.Lib.Mail
import Serials.Model.Invite (Invite)
import Serials.Model.Types (EmailAddress(..))
import Serials.Model.App (readAllEnv)
import Serials.AppMonad

import Servant.Server

import Text.Blaze.Html5 hiding (code)
import Text.Blaze.Html5.Attributes

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

signup :: UserSignup -> App (Either Text User)
signup u = validate (validateSignup u) $ do
  time <- liftIO $ getCurrentTime
  mhash <- liftIO $ hashPassword (password u)
  case mhash of
    Nothing -> return $ Left "Could not hash user password"
    Just hash -> do
      let user = newUser u time hash
      createdUser <- User.insert user
      Invite.markUsed (code u) (User.id createdUser)
      sendWelcomeEmail createdUser
      return $ Right createdUser

newUser :: UserSignup -> UTCTime -> Text -> User
newUser u time hash = User.User {
    User.id             = pack "",
    User.firstName      = firstName u,
    User.lastName       = lastName u,
    User.email          = EmailAddress $ toLower $ email u,
    User.resetToken     = Nothing,
    User.hashedPassword = hash,
    User.admin          = False,
    User.created        = time
  }

customHashPolicy :: HashingPolicy
customHashPolicy = HashingPolicy 10 "$2b$"

hashPassword :: Text -> IO (Maybe Text)
hashPassword pw = do
  hash <- hashPasswordUsingPolicy customHashPolicy $ encodeUtf8 pw
  return $ decodeUtf8 <$> hash

-- Email -----------------------------------------------------------

sendWelcomeEmail :: User -> App ()
sendWelcomeEmail u = do
  ep <- asks (endpoint . env)
  sendMail [User.email u] (welcomeEmail ep u)

welcomeEmail :: Endpoint -> User -> Email
welcomeEmail ep u = Email "Your account is active!" $ do
  logoPage ep $ do
    h3 ("Hello " >> toHtml (User.firstName u) >> ", welcome to Web Fiction!")
    p $ do
      a ! href (textValue $ ep <> "/app#/login") $ "Click here to start reading"


-- Password Resetting ----------------------------------------------
forgotPassword :: EmailAddress -> App ()
forgotPassword email = do
  token <- liftIO $ Invite.generateCode
  User.addResetToken email token
  ep <- askEndpoint
  sendMail [email] (passwordEmail ep token)

resetPassword :: Text -> Text -> App (Either Text ())
resetPassword token pw = do
  mu <- User.findByToken token
  case mu of
    Nothing -> return $ Left "Invalid Token"
    Just u  -> do
      mhash <- liftIO $ hashPassword pw
      case mhash of
        Nothing -> return $ Left "Could not hash password"
        Just hash -> do
          let user = u { User.hashedPassword = hash, User.resetToken = Nothing }
          User.save (User.id user) user
          return $ Right ()

-- you need the token in the url, at least
passwordEmail :: Endpoint -> Text -> Email
passwordEmail ep token = Email "Reset your password" $ do
  logoPage ep $ do
    h3 "Reset your password"
    p $ do
      a ! href (textValue $ ep <> "/app#/password/reset/" <> token) $ "Click here to reset your password"


-- Validation --------------------------------------------------
-- maybe there's a library function for this
validate :: (Monad m) => m (Either Text ()) -> m (Either Text a) -> m (Either Text a)
validate validator rest = do
  result <- validator
  case result of
    Left err -> return $ Left err
    Right _  -> rest

validatePassword :: UserSignup -> Either Text ()
validatePassword u =
  if (password u) /= (passwordConfirmation u)
  then Left "Password and Password Confirmation do not match"
  else validateRequired "Password" (password u)

validateEmail :: UserSignup -> App (Either Text ())
validateEmail u = do
  existUser <- User.findByEmail $ toLower $ email u
  return $ if isJust existUser
    then Left "User already exists with that email"
    else Right ()

validateInvite :: UserSignup -> App (Either Text ())
validateInvite u = do
  mInvite <- Invite.find (code u)
  case mInvite of
    Nothing -> return $ Left "Invite not found"
    Just invite ->
      return $ if isJust (Invite.signup invite)
        then Left "Invite already used"
        else Right ()

validateRequired :: Text -> Text -> Either Text ()
validateRequired name txt =
  if Text.length txt == 0
  then Left $ name <> " is required"
  else Right ()

validateSignup :: UserSignup -> App (Either Text ())
validateSignup u = do
  errs <- lefts <$> sequence validators
  return $ case errs of
    []   -> Right ()
    x:xs -> Left x

  where
  validators = [
      return $ validateRequired "First Name" (firstName u),
      return $ validateRequired "Last Name" (firstName u),
      return $ validatePassword u,
      validateEmail u,
      validateInvite u
    ]

