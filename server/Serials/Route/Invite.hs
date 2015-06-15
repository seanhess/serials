{-# LANGUAGE OverloadedStrings #-}

module Serials.Route.Invite where

import Data.Pool
import Data.Time
import Data.Text (Text)
import Data.Monoid ((<>))
import Database.RethinkDB.NoClash (RethinkDBHandle)

import qualified Serials.Model.Invite as Invite
import Serials.Model.Invite (EmailAddress, Invite, InviteCode)
import Serials.Lib.Mail

import Servant.Server (ServantErr(..), err400)
import qualified Serials.Model.User as User
import qualified Serials.Model.Invite as Invite
import Serials.Model.App (readAllEnv, Env(..), Endpoint)

import Text.Blaze.Html5 hiding (style)
import Text.Blaze.Html5.Attributes

-- I need to validate the email address
inviteAddEmail :: Pool RethinkDBHandle -> EmailAddress -> IO (Either ServantErr ())
inviteAddEmail h e = do
  if not $ isValidAddress e
  then return $ Left $ err400 { errBody = "Invalid Email Address" }
  else do
    time <- getCurrentTime
    inv <- Invite.invite e

    -- for now, automatically send the invite email
    sendInviteEmail inv
    let inv' = inv { Invite.sent = Just time }

    Invite.add h inv'
    return $ Right ()

inviteSend :: Pool RethinkDBHandle -> InviteCode -> IO ()
inviteSend h c = do
  mi <- Invite.find h c
  case mi of
    Nothing -> return ()
    Just i  -> do
      sendInviteEmail i
      Invite.markSent h c


sendInviteEmail :: Invite -> IO ()
sendInviteEmail i = do
  env <- readAllEnv
  sendMail [Invite.email i] (inviteEmail i (endpoint env))

inviteEmail :: Invite -> Endpoint -> Email
inviteEmail i endpoint = Email "Invite to join Serials" $ do
  logoPage endpoint $ do
    h3 "You have been invited to join Serials"
    p $ do
      a ! href (textValue $ endpoint <> "/#/signup/" <> Invite.code i) $ "Click here to create an account"

