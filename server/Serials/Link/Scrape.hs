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

-- everything in this one file


data HTMLContent = HTMLAnchor URL Title | HTMLTitle Title deriving (Show)

parseToc :: URL -> Selector Text -> [Tag Text] -> [Content]
parseToc base sel tags = links
  where
    anchors = scrape (scrapeContent sel) tags
    links   = map (htmlToContent base) $ fromMaybe [] anchors
    --clean   = filter ((>0) . length . contentText) links

scrapeContent :: Selector Text -> Scraper Text [HTMLContent]
scrapeContent sel = chroot sel $ chroots (p // Any) (scrapeAnchor <|> scrapeTitle)

-- I want this to FAIL if it isn't an exact match
scrapeAnchor :: Scraper Text HTMLContent
scrapeAnchor = do
  url <- attr "href" a
  title <- text a
  return $ HTMLAnchor url title

-- I want it to fail if there isn't any text though
-- pure? MonadPlus? I'll figure it out
scrapeTitle :: Scraper Text HTMLContent
scrapeTitle = do
  title <- text Any
  return $ HTMLTitle title

htmlToContent :: URL -> HTMLContent -> Content
htmlToContent base (HTMLAnchor url title) = cleanLink (base </> url) title
htmlToContent _ (HTMLTitle title) = cleanTitle title

---------------------------------------------------------------------------

selector :: Text -> Selector Text
selector = soupSelector . css

soupSelector :: CSSSelector -> Selector Text
soupSelector = sel
  where
    sel (ID id)     = Any @: [("id" :: Text) @= id]
    sel (Class cls) = Any @: [hasClass cls] 
    sel (Tag tag)   = tag @: []

a :: Text
a = "a"

strong :: Text
strong = "strong"

p :: Text
p = "p"

button :: Text
button = "button"

---------------------------------------------------------------------------

