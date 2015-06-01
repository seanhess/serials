{-# LANGUAGE OverloadedStrings #-}

module Serials.Route.Invite where

import Data.Pool
import Data.Time
import Database.RethinkDB.NoClash

import qualified Serials.Model.Invite as Invite
import Serials.Model.Invite (Email, Invite, InviteCode)
import Serials.Lib.Mail

inviteAddEmail :: Pool RethinkDBHandle -> MailConfig -> Email -> IO ()
inviteAddEmail h cfg e = do
  time <- getCurrentTime
  inv <- Invite.invite e (Just time)
  Invite.add h inv
  sendInviteEmail inv cfg

inviteSend :: Pool RethinkDBHandle -> MailConfig -> InviteCode -> IO ()
inviteSend h cfg c = do
  mi <- Invite.find h c
  case mi of
    Nothing -> return ()
    Just i  -> do
      sendInviteEmail i cfg
      Invite.markSent h c



