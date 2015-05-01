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

--parseToc :: URL -> CSSSelector -> [Tag] -> [Content]
--parseToc base sel = catMaybes . map blockContent . chunks chunkTOC . select sel


    -- TODO htmlToContent
    --links   = map (htmlToContent base) $ fromMaybe [] anchors

    -- TODO clean
    --clean   = filter ((>0) . length . contentText) links

--testFriendship = tocLinks "http://www.fimfiction.net/story/62074/friendship-is-optimal" (selector ".chapters")

linkMatcher :: TagMatcher Content
linkMatcher = TagMatcher start end blockLink
  where start = match [(~== (open "a"))] 
        end   = match [(~== (close "a"))]

-- I want to match text inside an 
titleMatcher :: (Tag -> Bool) -> TagMatcher Content
titleMatcher m = TagMatcher start end blockTitle
  where start = match [m, isText] 
        end   = match [(~== (close ""))]

wormTitleMatcher = titleMatcher (~== open "strong")

----------------------------------------------------------------------

blockContent :: [Tag] -> Maybe Content
blockContent [] = Nothing
blockContent (t:ts)
  | isTagText t = blockTitle (t:ts)
  | otherwise   = blockLink (t:ts)

blockLink :: [Tag] -> Maybe Content
blockLink (t:ts) = do
    href <- maybeAttr "href" t
    let txt = allText ts
    guard (length txt > 0)
    return $ Link href txt

blockTitle :: [Tag] -> Maybe Content
blockTitle ts = do
    let txt = allText ts
    guard (length txt > 0)
    return $ Title txt

-------------------------------------------------------------------

htmlToContent :: URL -> HTMLContent -> Content
htmlToContent base (HTMLAnchor url title) = cleanLink (base </> url) title
htmlToContent _ (HTMLTitle title) = cleanTitle title

---------------------------------------------------------------------------

