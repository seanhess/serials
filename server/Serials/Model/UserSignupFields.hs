{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Serials.Model.UserSignupFields where

import Prelude hiding (id)

import Control.Applicative
import Data.Text (Text)

import Data.Aeson (ToJSON, FromJSON)

import GHC.Generics
import Database.RethinkDB.NoClash hiding (table)

data UserSignupFields = UserSignupFields {
  firstName :: Text
  , lastName :: Text
  , email :: Text
  , password :: Text
  , passwordConfirmation :: Text
} deriving (Show, Generic)

instance FromJSON UserSignupFields
instance ToJSON UserSignupFields
instance FromDatum UserSignupFields
instance ToDatum UserSignupFields

