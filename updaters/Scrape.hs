{-# LANGUAGE OverloadedStrings #-}

module Scrape where

import Links

import Prelude hiding (id)

import Control.Applicative

import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromMaybe, fromJust)
import Data.Monoid ((<>))

import Text.HTML.Scalpel hiding (URL)
import Text.HTML.TagSoup
import Text.StringLike hiding (fromString, toString)


---------------------------------------------------------------------------
-- toc sites here

testPact = tocLinks "https://pactwebserial.wordpress.com/table-of-contents/" (selector ".entry-content")
testWorm = tocLinks "https://parahumans.wordpress.com/table-of-contents/" (selector ".entry-content")
testHPMOR = tocLinks "http://hpmor.com/" (selector ".toclist")

data HTMLAnchor = HTMLAnchor URL Title deriving (Show)

tocLinks :: URL -> Selector ByteString -> IO [Link]
tocLinks url sel = do
    body <- downloadBody url
    let tags = parseTags body
        as = scrape (scrapeAnchors sel) tags
        ls = map (anchorToLink url) $ fromMaybe [] as
    return $ ls

scrapeAnchors :: Selector ByteString -> Scraper ByteString [HTMLAnchor]
scrapeAnchors sel = chroot sel $ chroots a scrapeAnchor

scrapeAnchor :: Scraper ByteString HTMLAnchor
scrapeAnchor = do
  url <- attr "href" Any
  title <- text Any
  return $ HTMLAnchor (decodeBS url) (decodeBS title)

anchorToLink :: URL -> HTMLAnchor -> Link
anchorToLink base (HTMLAnchor url title) = Link (base </> url) title

---------------------------------------------------------------------------

scrapeTitle :: Scraper ByteString ByteString
scrapeTitle = text ("title" :: ByteString)

selector :: Text -> Selector ByteString
selector = sel . parseSelector
  where
    sel (ID id)     = Any @: [("id" :: ByteString) @= encodeBS id]
    sel (Class cls) = Any @: [hasClass (encodeBS cls)] 
    sel (Tag tag)   = (encodeBS tag) @: []

a :: ByteString
a = "a"

p :: ByteString
p = "p"

button :: ByteString
button = "button"

---------------------------------------------------------------------------

