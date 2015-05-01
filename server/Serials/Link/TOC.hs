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

linkMatcher = TagMatcher start end
  where start = match [(~== (open "a"))] 
        end   = match [(~== (close "a"))]

-- I want to match text inside an 
wormTitleMatcher = TagMatcher start end
  where start = match [(~== (open "strong")), isText] 
        end   = match [(~== (close ""))]

goo :: [Tag]
goo = [TagOpen "strong" [],TagText "Arc 1: Gestation",TagOpen "br" [],TagClose "br",TagText "\n",TagClose "strong",TagOpen "strong" [],TagOpen "a" [("href","https://parahumans.wordpress.com/category/stories-arcs-1-10/arc-1-gestation/1-01/")],TagText "1.01",TagOpen "br" [],TagClose "br",TagText "\n",TagClose "a",TagClose "strong",TagOpen "strong" [],TagOpen "a" [("href","https://parahumans.wordpress.com/category/stories-arcs-1-10/arc-1-gestation/1-02/")],TagText "1.02",TagOpen "br" [],TagClose "br",TagText "\n",TagClose "a",TagClose "strong",TagOpen "strong" [],TagOpen "a" [("href","https://parahumans.wordpress.com/category/stories-arcs-1-10/arc-1-gestation/1-03/")],TagText "1.03",TagClose "a",TagOpen "a" [("href","https://parahumans.wordpress.com/category/stories-arcs-1-10/arc-1-gestation/1-03/")],TagOpen "br" [],TagClose "br",TagText "\n",TagClose "a",TagClose "strong",TagOpen "strong" [],TagOpen "a" [("href","https://parahumans.wordpress.com/category/stories-arcs-1-10/arc-1-gestation/1-04/")],TagText "1.04",TagOpen "br" [],TagClose "br",TagText "\n",TagClose "a",TagClose "strong",TagOpen "strong" [],TagOpen "a" [("href","https://parahumans.wordpress.com/category/stories-arcs-1-10/arc-1-gestation/1-05/")],TagText "1.05",TagClose "a",TagOpen "a" [("href","https://parahumans.wordpress.com/category/stories-arcs-1-10/arc-1-gestation/1-05/")],TagOpen "br" [],TagClose "br",TagText "\n",TagClose "a",TagClose "strong",TagOpen "strong" [],TagOpen "a" [("href","https://parahumans.wordpress.com/category/stories-arcs-1-10/arc-1-gestation/1-06/")],TagText "1.06",TagClose "a",TagOpen "a" [("href","https://parahumans.wordpress.com/category/stories-arcs-1-10/arc-1-gestation/1-x-interlude/")],TagOpen "br" [],TagClose "br",TagText "\n1.x (Interlude; Danny)",TagOpen "br" [],TagClose "br",TagText "\n",TagClose "a",TagClose "strong",TagClose "p",TagText "\n",TagOpen "p" []]

-- TODO need a way to match text inside of something...
-- but it isn't  ... 

----------------------------------------------------------------------

--chunkTOC :: [Tag] -> ([Tag], [Tag])
--chunkTOC = chunk start end
  --where
  --start = (~== (open "a")) <||> isText
  --end   = (~== (open "a")) <||> (~== (close "a"))

  --start = (~== (open "a"))
  --end   = (~== (close "a"))

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

