
module RSS where

import Links

import Control.Monad
import Control.Applicative

import Data.Maybe

import Text.Feed.Import
import Text.Feed.Query
import Text.Feed.Types

import Text.XML.Light (Element)
import Text.XML.Light.Input
import Text.XML.Light.Lexer ( XmlSource )

import qualified Data.ByteString.Lazy as BL

twigURL = "https://twigserial.wordpress.com/feed/"

rssLinks :: URL -> IO [Link]
rssLinks feedURL = do
    body <- downloadBody feedURL
    let ls = case parse body of 
              Nothing -> []
              Just f  -> feedLinks f
    return ls

parse :: BL.ByteString -> Maybe Feed
parse content = do
    doc <- parseXMLDoc content
    feed <- readFeed doc
    return feed

readFeed :: Element -> Maybe Feed
readFeed e =
  readAtom e `mplus`
  readRSS2 e `mplus`
  readRSS1 e `mplus`
  Just (XMLFeed e)

feedLinks :: Feed -> [Link]
feedLinks = catMaybes . map itemLink . feedItems

itemLink :: Item -> Maybe Link
itemLink item = Link <$> 
                getItemLink item <*> 
                getItemTitle item <*> 
                getItemPublishDateString item


-- well, that's not going to work, is it? :)
