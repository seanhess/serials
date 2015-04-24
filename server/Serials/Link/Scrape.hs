{-# LANGUAGE OverloadedStrings #-}

module Serials.Link.Scrape where

import Serials.Link.Link

import Prelude hiding (id, length)

import Control.Applicative

import Data.Text (Text, length)
import qualified Data.Text as T
import Data.Maybe (fromMaybe, fromJust)
import Data.Monoid ((<>))

import Text.HTML.Scalpel hiding (URL)
import Text.HTML.TagSoup
import Text.StringLike hiding (fromString, toString)


---------------------------------------------------------------------------
-- toc sites here
-- should I drop empty title links? Sure, let's try it. It'll fix friendship too

data HTMLAnchor = HTMLAnchor URL Title deriving (Show)


parseToc :: URL -> Selector Text -> [Tag Text] -> [Link]
parseToc base sel tags = clean
  where
    anchors = scrape (scrapeAnchors sel) tags
    links   = map (anchorToLink base) $ fromMaybe [] anchors
    clean   = filter ((>0) . length . linkTitle) links

scrapeAnchors :: Selector Text -> Scraper Text [HTMLAnchor]
scrapeAnchors sel = chroot sel $ chroots a scrapeAnchor

scrapeAnchor :: Scraper Text HTMLAnchor
scrapeAnchor = do
  url <- attr "href" Any
  title <- text Any
  return $ HTMLAnchor url title

anchorToLink :: URL -> HTMLAnchor -> Link
anchorToLink base (HTMLAnchor url title) = link (base </> url) title

---------------------------------------------------------------------------

scrapeTitle :: Scraper Text Text
scrapeTitle = text ("title" :: Text)

selector :: Text -> Selector Text
selector = sel . parseSelector
  where
    sel (ID id)     = Any @: [("id" :: Text) @= id]
    sel (Class cls) = Any @: [hasClass cls] 
    sel (Tag tag)   = tag @: []

a :: Text
a = "a"

p :: Text
p = "p"

button :: Text
button = "button"

---------------------------------------------------------------------------

