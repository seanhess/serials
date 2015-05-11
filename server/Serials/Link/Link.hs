{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Serials.Link.Link where

import Prelude hiding (dropWhile)

import Data.Aeson (FromJSON, ToJSON)
import Data.Text hiding (map, zip, elem)
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe, fromJust)
import Data.List (elem)

import Control.Lens ((^.))

import GHC.Generics
import Network.URI

type Title = Text
type DateString = Text
type URL = Text

-- no, represent my friend, represent!
data Content = Link {
  linkURL :: URL,
  linkText :: Title
} | Title {
  titleText :: Title
} deriving (Show, Eq, Generic)

instance FromJSON Content
instance ToJSON Content

cleanLink :: URL -> Title -> Content
cleanLink u t = Link (clean u) (clean t)

cleanTitle :: Title -> Content
cleanTitle t = Title (clean t)

clean :: Text -> Text
clean = strip

contentText :: Content -> Text
contentText (Link _ txt) = txt
contentText (Title txt) = txt

--addNumbers :: [Link] -> [Link]
--addNumbers ls = map addNumber $ zip [1..] ls
  --where
    --addNumber (n, l) = l { linkNumber = n }

---------------------------------------------------------------------

-- use relative URI parsing instead!
-- if you can't parse it, just use the path they provided
(</>) :: URL -> URL -> URL
(</>) base path = fromMaybe path $ do
    b <- parseURIReference $ unpack $ strip base
    p <- parseURIReference $ unpack $ strip path
    return $ pack $ show $ p `relativeTo` b

infixr 6 </>

