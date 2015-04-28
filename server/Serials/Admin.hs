{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Serials.Admin where

import Shelly
import qualified Data.Text as T
import Data.Text (Text)
import Data.Monoid ((<>))
import Data.Aeson

import GHC.Generics

default (T.Text)


data Log = Log {
  text :: Text
} deriving (Show, Eq, Generic)

instance ToJSON Log

importLog :: Int -> IO Log
importLog n = shelly $ errExit False $ do
  res <- run "tail" ["-n", (T.pack $ show n), "/var/log/serials.log"]
  code <- lastExitCode
  err <- lastStderr
  return $ Log $ case code of 
    0 -> res
    n -> "Error: exited with code: " <> (T.pack $ show n) <> "\n" <> err


