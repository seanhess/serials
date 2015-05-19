{-# LANGUAGE OverloadedStrings #-}

module Serials.Route.Invite where

import Data.Pool
import Data.Time
import Database.RethinkDB.NoClash

import qualified Serials.Model.Invite as Invite
import Serials.Model.Invite (Email, Invite, InviteCode)
import Serials.Lib.Mail

inviteAddEmail :: Pool RethinkDBHandle -> Email -> IO ()
inviteAddEmail h e = do
  time <- getCurrentTime
  inv <- Invite.invite e (Just time)
  Invite.add h inv
  sendInviteEmail inv

inviteSend :: Pool RethinkDBHandle -> InviteCode -> IO ()
inviteSend h c = do
  mi <- Invite.find h c
  case mi of
    Nothing -> return ()
    Just i  -> do
      sendInviteEmail i
      Invite.markSent h c



