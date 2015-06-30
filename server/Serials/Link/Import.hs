{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Serials.Link.Import where

import Prelude hiding (null)

import Control.Lens ((^.))
import Control.Applicative

import Data.Data
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromMaybe, fromJust)
import Data.Text (unpack, Text, null, isInfixOf)
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
import Text.Regex.PCRE

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

menuContent :: URL -> CSSSelector -> CSSSelector -> URL -> IO [Content]
menuContent base start end url = parseMenuContent base start end <$> downloadTags url

-- if it's empty, then don't select the text...
tocContent :: CSSSelector -> Maybe CSSSelector -> URL -> IO [Content]
tocContent root title url = parseToc url root title <$> downloadTags url

-- yeah, because you need to map a source to an IO action
importContent :: URL -> ImportSettings -> IO [Content]
importContent url set = fetchLinks set url
  where
    fetchLinks (MenuSettings base open) = menuContent base (css open) (css "select")
    fetchLinks (TOCSettings root title) = tocContent (css root) (emptySelector title)
    emptySelector t = if null t then Nothing else Just (css t)

-- baby steps:
-- don't require a root selector
-- only have the admin see them
-- set it to "body" and "blank", then go in an clean it up yourself? But then it'll suck...

-- don't have them specify chapters at all. Just put in the url, the title, etc, and it submits. Then I can clean it up.

-- guesses the import method based on the url and other content
-- make better algorithms for figuring it out
--urlContent :: URL -> IO [Content]
--urlContent url = do
  --if unpack url =~ "fanfiction.net" :: Bool
  --then 
  --else if unpack url =~ "fanfiction.net"

guessMethod :: URL -> IO [Content]
guessMethod url
  | isFanfictionNet url = fanfictionNetContent url
  | otherwise           = tocContent (css "body") Nothing url

fanfictionNetContent :: URL -> IO [Content]
fanfictionNetContent url = menuContent url (css "#chap_select") (css "select") url

isFanfictionNet :: URL -> Bool
isFanfictionNet url =
     "fanfiction.net" `isInfixOf` url
  || "fimfiction.net" `isInfixOf` url

-- How can I identify a table of contents on a page?
-- the links are all going to be next to each other, they don't have a lot of text in between them. They are usually entirely links. I don't want to MISS them though.

-- it would be so cool if I could train it...
-- identify what each one is
-- then it gets smarter about it over time.

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

--testTwig = mapM_ print =<< menuContent "https://twigserial.wordpress.com/donate/" "https://twigserial.wordpress.com/?cat=" (css "#cat") (css "select")
--testGinny = mapM_ print =<< menuContent "http://fanfiction.net/s/11117811/" "http://fanfiction.net/s/11117811/" (css "#chap_select") (css "select")

--testPact = mapM_ print =<< tocContent "https://pactwebserial.wordpress.com/table-of-contents/" (css ".entry-content") (Just $ css "strong")
--testWorm = mapM_ print =<< tocContent "https://parahumans.wordpress.com/table-of-contents/" (css ".entry-content") (Just $ css "strong")
--testHPMOR = mapM_ print =<< tocContent "http://hpmor.com/" (css ".toclist") Nothing

--testFriendship = mapM_ print =<< tocContent "http://www.fimfiction.net/story/62074/friendship-is-optimal" (css ".chapters") Nothing

--------------------------------------------------------------
-- Test some stuff!


