{-# LANGUAGE OverloadedStrings #-}

module Serials.Route.Invite where

import Data.Pool
import Data.Time
import Database.RethinkDB.NoClash

import qualified Serials.Model.Invite as Invite
import Serials.Model.Invite (Email, Invite, InviteCode)
import Serials.Lib.Mail hiding (Email)

import Servant.Server (ServantErr(..), err400)

-- I need to validate the email address
inviteAddEmail :: Pool RethinkDBHandle -> Email -> IO (Either ServantErr ())
inviteAddEmail h e = do
  if not $ isValidAddress e
  then return $ Left $ err400 { errBody = "Invalid Email Address" }
  else do
    time <- getCurrentTime
    inv <- Invite.invite e Nothing
    -- don't automatically send email, wait until manually approved
    -- sendInviteEmail inv
    Invite.add h inv
    return $ Right ()

inviteSend :: Pool RethinkDBHandle -> InviteCode -> IO ()
inviteSend h c = do
  mi <- Invite.find h c
  case mi of
    Nothing -> return ()
    Just i  -> do
      sendInviteEmail i
      Invite.markSent h c



