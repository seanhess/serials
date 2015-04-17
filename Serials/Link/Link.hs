{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Serials.Link.Link where

import Prelude hiding (dropWhile)

import Data.Aeson (FromJSON, ToJSON)
import Data.Text
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe, fromJust)

import Control.Lens ((^.))

import GHC.Generics

type Title = Text
type DateString = Text
type URL = Text

data Link = Link {
  linkURL :: URL,
  linkTitle :: Title
} deriving (Show, Eq, Generic)

instance FromJSON Link
instance ToJSON Link

link :: URL -> Title -> Link
link u t = Link (clean u) (clean t)

clean :: Text -> Text
clean = strip

---------------------------------------------------------------------

(</>) :: URL -> URL -> URL
(</>) base path = (dropWhileEnd (=='/') base) <> "/" <> dropWhile (=='/') path
infixr 6 </>

----------------------------------------------------------------------

data CSSSelector = ID Text | Class Text | Tag Text deriving (Show, Eq)

parseSelector :: Text -> CSSSelector
parseSelector xs = (sel . fromJust . uncons) xs
  where
    sel ('#',id) = ID id
    sel ('.',cls) = Class cls
    sel _   = Tag xs

