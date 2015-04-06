module Types where

import Text.HTML.Scalpel

type Title = String

data Link = Link {
  linkURL :: URL,
  linkTitle :: Title
} deriving (Show, Eq)
