{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Serials.Link.Import where

import Control.Lens ((^.))
import Control.Applicative

import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromMaybe, fromJust)
import Data.Text (unpack, Text)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy (toStrict, fromStrict)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8, decodeLatin1)

import GHC.Generics (Generic)

import Network.Wreq hiding (Link)

--import Serials.Link.Scrape
--import Serials.Link.Parse
import Serials.Link.TOC
import Serials.Link.Link
import Serials.Link.Soup

import Text.HTML.Scalpel hiding (URL)
import Text.HTML.TagSoup hiding (Tag)

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

--menuContent :: URL -> URL -> TagSelector -> TagSelector -> IO [Content]
--menuContent url base start end = do
    --body <- downloadBody url
    --let tags = parseTags body
        --select = selectMenu start end
    --return $ parseMenuContent base select tags

-- if it's empty, then don't select the text...
tocContent :: URL -> CSSSelector -> Maybe CSSSelector -> IO [Content]
tocContent url rootSelector titleSelector = 
    parseToc url rootSelector titleSelector <$> downloadTags url

-- yeah, because you need to map a source to an IO action
importContent :: URL -> ImportSettings -> IO [Content]
importContent url set = fetchLinks set
  where
    fetchLinks = undefined
    --fetchLinks (MenuSettings base open close) = menuContent url base (openSelector open) (closeSelector close)
    --fetchLinks (TOCSettings cssQuery) = tocContent url (selector cssQuery)

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

--testTwig = menuLinks "https://twigserial.wordpress.com/donate/" "https://twigserial.wordpress.com/?cat=" (openSelector "#cat") (closeSelector "select")
--testGinny = menuLinks "http://fanfiction.net/s/11117811/" "http://fanfiction.net/s/11117811/" (openSelector "#chap_select") (closeSelector "select")

testPact = mapM_ print =<< tocContent "https://pactwebserial.wordpress.com/table-of-contents/" (css ".entry-content") (Just $ css "strong")
testWorm = mapM_ print =<< tocContent "https://parahumans.wordpress.com/table-of-contents/" (css ".entry-content") (Just $ css "strong")
testHPMOR = mapM_ print =<< tocContent "http://hpmor.com/" (css ".toclist") Nothing

testFriendship = mapM_ print =<< tocContent "http://www.fimfiction.net/story/62074/friendship-is-optimal" (css ".chapters") Nothing

--------------------------------------------------------------
-- Test some stuff!


