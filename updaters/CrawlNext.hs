{-# LANGUAGE OverloadedStrings #-}

module CrawlNext where

import Links
import Scrape

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.UTF8 (toString)
import Data.List
import Data.Monoid ((<>))

import Prelude hiding (id)

import Text.HTML.Scalpel hiding (URL)
import Text.HTML.TagSoup
import Control.Applicative

-- how is it going to give you the next link?
-- you have to actually hit the page

-- scraping a single page gives you:
-- that page's title
-- the next page's url

data PageScrapeResult = PageScrapeResult (Maybe Title) (Maybe URL) deriving Show

nextLink :: URL -> Selector String -> IO [Link]
nextLink url sel = do
    body <- downloadBody url
    -- keep downloading the next url until we can't find the 
    return []


----------------------------------------------------------

testTwig = do
    body <- downloadBody "https://twigserial.wordpress.com/2015/03/17/taking-root-1-3/"
    let tags = parseTags body
        title = findTitle tags
        url = findNextLink "Next" (selector ".entry-content") tags
    print title
    print (url)

testFanfic = do
    body <- downloadBody "https://www.fanfiction.net/s/11117811/"
    let tags = parseTags body
    print $ findNextLink "Next >" (selector "#content_wrapper_inner") tags

testWorm = do
    body <- downloadBody "https://parahumans.wordpress.com/2011/06/14/gestation-1-2/"
    let tags = parseTags body
    print $ findNextLink "Next Chapter" (selector ".post") tags

---------------------------------------------------------
-- scrapers


findTitle :: [Tag ByteString] -> Maybe Title
findTitle tags = toString <$> scrape scrapeTitle tags

findNextLink :: String -> Selector ByteString -> [Tag ByteString] -> Maybe (String, URL)
findNextLink m sel tags = do
    ls <- findLinks sel tags
    find (matchLink m) ls

findLinks :: Selector ByteString -> [Tag ByteString] -> Maybe [(String, URL)]
findLinks sel tags = scrape (scrapeLinks sel) tags

matchLink :: String -> (String, URL) -> Bool
matchLink match (text, url) = match == text


scrapeLinks :: Selector ByteString -> Scraper ByteString [(String, URL)]
scrapeLinks sel = chroot sel $ do
    links <- scrapeAs
    buttons <- scrapeFanficButton
    return $ links <> buttons

scrapeAs :: Scraper ByteString [(String, URL)]
scrapeAs = chroots a $ do
    us <- attr "href" Any
    ts <- text Any
    return $ (toString ts, toString us)

scrapeFanficButton :: Scraper ByteString [(String, URL)]
scrapeFanficButton = chroots button $ do
    oc <- attr "onClick" Any
    t <- text Any
    let url = init $ dropWhile (/= '/') $ toString oc
    return $ (toString t, url)

