{-# LANGUAGE OverloadedStrings #-}

module CrawlNext where

import Links
import Scrape

import Control.Applicative

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.UTF8 (toString)
import Data.List
import Data.Monoid ((<>))
import Data.Maybe (fromJust)

import Prelude hiding (id)

import Text.HTML.Scalpel hiding (URL)
import Text.HTML.TagSoup
import Text.Regex.PCRE

import Network.URI

-- TODO scraping the wordpress sites is super slow. Is it CPU bound?

data CrawlSettings = CrawlSettings {
  crawlNextText :: String,
  crawlSelector :: Selector ByteString
}

twigSettings = CrawlSettings "" (selector ".nav-next")
fanficSettings = CrawlSettings "Next" (selector "#content_wrapper_inner")
pactSettings = CrawlSettings "Next" (selector ".entry-content")

----------------------------------------------------------

--testTwig = do
    --body <- downloadBody "https://twigserial.wordpress.com/2015/03/19/taking-root-1-4/"
    --let tags = parseTags body
        --title = findTitle tags
    --print $ findNextLink "" (selector ".nav-next") tags

--testFanfic = do
    --body <- downloadBody "https://www.fanfiction.net/s/11117811/"
    --let tags = parseTags body
    --print $ findNextLink "Next >" (selector "#content_wrapper_inner") tags

--testWorm = do
    --body <- downloadBody "https://parahumans.wordpress.com/2011/06/14/gestation-1-2/"
    --let tags = parseTags body
    --print $ findNextLink "Next Chapter" (selector ".post") tags

testTwig = crawlPages twigSettings "https://twigserial.wordpress.com/2014/12/24/taking-root-1-1/"
testGinny = crawlPages fanficSettings "https://www.fanfiction.net/s/11117811/"
testPact = crawlPages pactSettings "https://pactwebserial.wordpress.com/2013/12/17/bonds-1-1/"


---------------------------------------------------------

-- new plan! start at 1, and go through all of them until you don't find it
-- convert them to links as you go

scanPage :: CrawlSettings -> URL -> IO (Maybe Link, Maybe URL)
scanPage (CrawlSettings next sel) url = do
    putStrLn $ "Scanning: " <> show url
    body <- downloadBody url
    putStrLn $ " - done"
    let tags = parseTags body
    putStrLn $ " - parsed"
    let mt = findTitle tags
        u = findNextLink next sel tags >>= nextURL url
        link = case mt of 
          Just t -> Just $ Link url t
          Nothing -> Nothing
    return (link, u)

-- crawl until you drop!
crawlPages :: CrawlSettings -> URL -> IO [Link]
crawlPages set url = do
    putStrLn $ "------------------------------"
    (ml, mu) <- scanPage set url
    print ml
    print mu

    case ml of
      Nothing   -> return []
      Just link -> do
        case mu of
          Nothing  -> return [link]
          Just url -> do
            ls <- crawlPages set url
            return $ link : ls


-- parse and take the domain out then reapply
nextURL :: URL -> URL -> Maybe URL
nextURL base ('/':url) = case domain base of
                          Nothing -> Nothing
                          Just d  -> Just $ d <> "/" <> url
nextURL _ url = Just url

-- I'm just being lazy, parse the url!
domain :: URL -> Maybe URL
domain url = do
    uri <- parseURI url 
    auth <- uriAuthority uri
    return $ uriScheme uri <> "//" <> uriRegName auth <> uriPort auth




---------------------------------------------------------
-- scrapers


findTitle :: [Tag ByteString] -> Maybe Title
findTitle tags = toString <$> scrape scrapeTitle tags

findNextLink :: String -> Selector ByteString -> [Tag ByteString] -> Maybe URL
findNextLink m sel tags = do
    ls <- findLinks sel tags
    l  <- find (matchLink m) ls
    return $ snd l

findLinks :: Selector ByteString -> [Tag ByteString] -> Maybe [(String, URL)]
findLinks sel tags = scrape (scrapeNextLinks sel) tags

matchLink :: String -> (String, URL) -> Bool
matchLink match (text, url) = text =~ match


scrapeNextLinks :: Selector ByteString -> Scraper ByteString [(String, URL)]
scrapeNextLinks sel = chroot sel $ do
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

