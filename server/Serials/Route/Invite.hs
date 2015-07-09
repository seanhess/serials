{-# LANGUAGE OverloadedStrings #-}

module Serials.Route.Invite where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader

import Data.Pool
import Data.Time
import Data.Text (Text)
import Data.Monoid ((<>))
import Database.RethinkDB.NoClash (RethinkDBHandle)

import qualified Serials.Model.Invite as Invite
import Serials.Model.Invite (Invite, InviteCode)
import Serials.Lib.Mail

import Servant.Server (ServantErr(..), err400)
import qualified Serials.Model.User as User
import qualified Serials.Model.Invite as Invite
import Serials.Model.App (readAllEnv)
import Serials.Model.Types (EmailAddress(..))
import Serials.Types

import Text.Blaze.Html5 hiding (style)
import Text.Blaze.Html5.Attributes

-- I need to validate the email address
inviteAddEmail :: EmailAddress -> App (Either Text ())
inviteAddEmail e = do
  if not $ isValidAddress e
  then return $ Left $ "Invalid Email Address"
  else do
    time <- liftIO $ getCurrentTime
    inv <- liftIO $ Invite.invite e

    -- for now, automatically send the invite email
    sendInviteEmail inv
    let inv' = inv { Invite.sent = Just time }

    Invite.add inv'
    return $ Right ()

inviteSend :: InviteCode -> App ()
inviteSend c = do
  mi <- Invite.find c
  case mi of
    Nothing -> return ()
    Just i  -> do
      sendInviteEmail i
      Invite.markSent c


sendInviteEmail :: Invite -> App ()
sendInviteEmail i = do
  ep <- asks (endpoint . env)
  sendMail [Invite.email i] (inviteEmail i ep)

inviteEmail :: Invite -> Endpoint -> Email
inviteEmail i endpoint = Email "Invite to join Web Fiction" $ do
  logoPage endpoint $ do
    h3 "You have been invited to join Web Fiction"
    p $ do
      a ! href (textValue $ endpoint <> "/#/signup/" <> Invite.code i) $ "Click here to create an account"

