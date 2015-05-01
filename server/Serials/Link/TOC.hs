{-# LANGUAGE OverloadedStrings #-}

module Serials.Link.TOC where

import Serials.Link.Link

import Prelude hiding (id, length, readFile, writeFile)

import Data.Text (Text, length)
import Data.Text.IO (readFile, writeFile)
import qualified Data.Text as T
import Data.Maybe
import Data.Monoid ((<>))

import Control.Applicative
import Control.Monad


import Text.HTML.TagSoup hiding (Tag)
import Serials.Link.Soup

----------------------------------------------------------------------------

data HTMLContent = HTMLAnchor URL Title | HTMLTitle Title deriving (Show)

parseToc :: URL -> CSSSelector -> Maybe CSSSelector -> [Tag] -> [Content]
parseToc base sel mTitleSelector = toContent . matchersChunks matchers . select sel
  where
  toContent = map (htmlToContent base)
  matchers = catMaybes [Just linkMatcher, tMatcher]

  tMatcher :: Maybe (TagMatcher HTMLContent)
  tMatcher = titleMatcher <$> (matchSelector <$> mTitleSelector)

linkMatcher :: TagMatcher HTMLContent
linkMatcher = TagMatcher start end blockAnchor
  where start = match [(~== (open "a"))] 
        end   = match [(~== (close "a"))]

-- I want to match text inside an 
titleMatcher :: (Tag -> Bool) -> TagMatcher HTMLContent
titleMatcher m = TagMatcher start end blockTitle
  where start = match [m, isText] 
        end   = match [(~== (close ""))]

----------------------------------------------------------------------

blockAnchor :: [Tag] -> Maybe HTMLContent
blockAnchor (t:ts) = do
    href <- maybeAttr "href" t
    let txt = allText ts
    guard (length txt > 0)
    return $ HTMLAnchor href txt

blockTitle :: [Tag] -> Maybe HTMLContent
blockTitle ts = do
    let txt = allText ts
    guard (length txt > 0)
    return $ HTMLTitle txt

-------------------------------------------------------------------

htmlToContent :: URL -> HTMLContent -> Content
htmlToContent base (HTMLAnchor url title) = cleanLink (base </> url) title
htmlToContent _ (HTMLTitle title) = cleanTitle title

---------------------------------------------------------------------------

