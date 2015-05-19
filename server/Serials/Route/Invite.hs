{-# LANGUAGE OverloadedStrings #-}

module Serials.Route.Invite where

import Data.Pool
import Database.RethinkDB.NoClash

import Serials.Model.Invite
import Serials.Lib.Mail

invite :: Pool RethinkDBHandle -> Email -> IO ()
invite h e = do
  inv <- addEmail h e
  sendInviteEmail inv
  return ()

