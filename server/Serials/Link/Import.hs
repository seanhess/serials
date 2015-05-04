{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Serials.Link.Import where

import Prelude hiding (null)

import Control.Lens ((^.))
import Control.Applicative

import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromMaybe, fromJust)
import Data.Text (unpack, Text, null)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy (toStrict, fromStrict)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8, decodeLatin1)

import GHC.Generics (Generic)

import Network.Wreq hiding (Link)

--import Serials.Link.Scrape
import Serials.Link.Menu
import Serials.Link.TOC
import Serials.Link.Link
import Serials.Link.Soup

import Text.HTML.Scalpel hiding (URL)
import Text.HTML.TagSoup hiding (Tag, select)

data ImportSettings =
  MenuSettings {
    menuBase :: URL,
    menuOpen :: Text
  } |
  TOCSettings {
    tocSelector :: Text,
    titleSelector :: Text
  }
  deriving (Show, Eq, Generic)

menuContent :: URL -> URL -> CSSSelector -> CSSSelector -> IO [Content]
menuContent url base start end = parseMenuContent base start end <$> downloadTags url

-- if it's empty, then don't select the text...
tocContent :: URL -> CSSSelector -> Maybe CSSSelector -> IO [Content]
tocContent url root title = parseToc url root title <$> downloadTags url

-- yeah, because you need to map a source to an IO action
importContent :: URL -> ImportSettings -> IO [Content]
importContent url set = fetchLinks set
  where
    fetchLinks (MenuSettings base open) = menuContent url base (css open)(css "select")
    fetchLinks (TOCSettings root title) = tocContent url (css root) (emptySelector title)
    emptySelector t = if null t then Nothing else Just (css t)

--test = do
    --body <- downloadBody "http://www.fimfiction.net/story/62074/friendship-is-optimal"
    --let tags = parseTags body
    --mapM_ print $ parseToc "http://www.fimfiction.net/story/62074/friendship-is-optimal" (css ".chapters") $ tags

----------------------------------------------------------

downloadTags :: URL -> IO [Tag]
downloadTags url = parseTags <$> downloadBody url

downloadBody :: URL -> IO Text
downloadBody url = toStrict <$> downloadBodyLazy url

downloadBodyLazy :: URL -> IO TL.Text
downloadBodyLazy url = do
    r <- get (unpack url)
    let body = r ^. responseBody :: ByteString
    return $ decodeUtf8 body

-----------------------------------------------------------

testTwig = mapM_ print =<< menuContent "https://twigserial.wordpress.com/donate/" "https://twigserial.wordpress.com/?cat=" (css "#cat") (css "select")
testGinny = mapM_ print =<< menuContent "http://fanfiction.net/s/11117811/" "http://fanfiction.net/s/11117811/" (css "#chap_select") (css "select")

testPact = mapM_ print =<< tocContent "https://pactwebserial.wordpress.com/table-of-contents/" (css ".entry-content") (Just $ css "strong")
testWorm = mapM_ print =<< tocContent "https://parahumans.wordpress.com/table-of-contents/" (css ".entry-content") (Just $ css "strong")
testHPMOR = mapM_ print =<< tocContent "http://hpmor.com/" (css ".toclist") Nothing

testFriendship = mapM_ print =<< tocContent "http://www.fimfiction.net/story/62074/friendship-is-optimal" (css ".chapters") Nothing

--------------------------------------------------------------
-- Test some stuff!


