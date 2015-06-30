{-# LANGUAGE DeriveGeneric #-}

module Serials.Model.Types where

import Data.Text
import Data.Aeson
import GHC.Generics

newtype EmailAddress = EmailAddress Text deriving (Show, Eq, Generic)
instance ToJSON EmailAddress
instance FromJSON EmailAddress

emailValue :: EmailAddress -> Text
emailValue (EmailAddress e) = e
