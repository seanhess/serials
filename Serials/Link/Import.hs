{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Serials.Link.Import where

import Control.Lens ((^.))

import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromMaybe, fromJust)
import Data.Text (unpack, Text)
import Data.Text.Lazy (toStrict, fromStrict)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8, decodeLatin1)

import GHC.Generics (Generic)

import Network.Wreq hiding (Link)

import Serials.Link.Scrape
import Serials.Link.Parse
import Serials.Link.Link

import Text.HTML.Scalpel hiding (URL)
import Text.HTML.TagSoup

data ImportSettings = 
  MenuSettings {
    menuBase :: URL,
    menuOpen :: Text,
    menuClose ::  Text
  } |
  TOCSettings {
    tocSelector :: Text
  }
  deriving (Show, Eq, Generic)

menuLinks :: URL -> URL -> TagSelector -> TagSelector -> IO [Link]
menuLinks url base start end = do
    body <- downloadBody url
    let tags = parseTags body
        select = selectMenu start end
    return $ parseMenuLinks base select tags

tocLinks :: URL -> Selector Text -> IO [Link]
tocLinks url sel = do
    body <- downloadBody url
    let tags = parseTags body
        as = scrape (scrapeAnchors sel) tags
        ls = map (anchorToLink url) $ fromMaybe [] as
    return $ ls

-- yeah, because you need to map a source to an IO action
links :: URL -> ImportSettings -> IO [Link]
links url (MenuSettings base open close) = menuLinks url base (openSelector open) (closeSelector close)
links url (TOCSettings cssQuery) = tocLinks url (selector cssQuery)

----------------------------------------------------------

downloadBody :: URL -> IO Text
downloadBody url = do
    r <- get (unpack url)
    let body = r ^. responseBody :: ByteString
    return $ (toStrict . decodeUtf8) body

-----------------------------------------------------------

testTwig = menuLinks "https://twigserial.wordpress.com/donate/" "https://twigserial.wordpress.com/?cat=" (openSelector "#cat") (closeSelector "select")
testGinny = menuLinks "http://fanfiction.net/s/11117811/" "http://fanfiction.net/s/11117811/" (openSelector "#chap_select") (closeSelector "select")

testPact = tocLinks "https://pactwebserial.wordpress.com/table-of-contents/" (selector ".entry-content")
testWorm = tocLinks "https://parahumans.wordpress.com/table-of-contents/" (selector ".entry-content")
testHPMOR = tocLinks "http://hpmor.com/" (selector ".toclist")

testFriendship = tocLinks "http://www.fimfiction.net/story/62074/friendship-is-optimal" (selector ".chapters")
