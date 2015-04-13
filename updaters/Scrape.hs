{-# LANGUAGE OverloadedStrings #-}

module Scrape where

import Links

import Prelude hiding (id)

import Control.Applicative

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.UTF8 (toString, fromString)
import Data.Maybe (fromMaybe)

import Text.HTML.Scalpel hiding (URL)
import Text.HTML.TagSoup
import Text.StringLike hiding (fromString, toString)


---------------------------------------------------------------------------
-- toc sites here

testPact = tocLinks "https://pactwebserial.wordpress.com/table-of-contents/" (selector ".entry-content")
testWorm = tocLinks "https://parahumans.wordpress.com/table-of-contents/" (selector ".entry-content")

tocLinks :: URL -> Selector ByteString -> IO [Link]
tocLinks url sel = do
    body <- downloadBody url
    let tags = parseTags body
        links = scrape (scrapeLinks sel) tags
    return $ fromMaybe [] links

scrapeLinks :: Selector ByteString -> Scraper ByteString [Link]
scrapeLinks sel = chroot sel $ chroots a scrapeLink

scrapeLink :: Scraper ByteString Link
scrapeLink = do
  url <- attr "href" Any
  title <- text Any
  return $ Link (toString title) (toString url)

scrapeOption :: Scraper ByteString Link
scrapeOption = do
  url <- attr "href" Any
  title <- text Any
  return $ Link (toString title) (toString url)

-- options: screen certain text, certain urls
-- woooo

---------------------------------------------------------------------------

scrapeTitle :: Scraper ByteString ByteString
scrapeTitle = text ("title" :: ByteString)

selector :: String -> Selector ByteString
selector ('#':id) = Any @: [("id" :: ByteString) @= fromString id]
selector ('.':cls) = Any @: [hasClass (fromString cls)] 
selector tag = (fromString tag) @: []

a :: ByteString
a = "a"

p :: ByteString
p = "p"

button :: ByteString
button = "button"

---------------------------------------------------------------------------

-- many of them have arcs

type Id = String
type Class = String

data Entry = Entry Title URL deriving (Show, Eq)
data Source = Source URL (Selector String)

parseEntries :: Selector String -> Scraper String [Entry]
parseEntries sel = chroots sel parseEntry

parseEntry :: Scraper String Entry
parseEntry = parseOption <|> parseLink

parseLink :: Scraper String Entry
parseLink = do
  url <- attr "href" Any
  title <- text Any
  return $ Entry title url

parseOption :: Scraper String Entry
parseOption = do
  return $ Entry "title" "url"
  --url <- attr "value" Any
  --title <- text Any
  --return $ Entry title url

--worm = Source "https://parahumans.wordpress.com/" (selector "#categories-2" // "a")
--pact = Source "https://pactwebserial.wordpress.com/table-of-contents/" (selector "#categories-2" // "a")
--ginny = Source "https://www.fanfiction.net/s/11117811/" (selector "#chap_select")

-- TWIG:
-- shoot, I totally can't parse this. Do a fanctiction next, then an rss feed
-- actually RSS should probably be the default
-- the RSS feeds have content in them! WTF?


--wormEntries = scrapeEntries (Any @: ["id" @= "categories-2"] // "a")
--pactUrl = 
--pactEntries = wormEntries
-- scrapeEntries (Any @: [hasClass "entry-content"] // "a")

-- root selector
-- then a tag selector
-- then 

-- is it because it isn't valid XML? They broke the rules so it just takes it out
-- maybe I should be using regexes instead...
-- it wouldn't be that hard.


scrapeSource :: Source -> IO (Maybe [Entry])
scrapeSource (Source url sel) = scrapeURL url (parseEntries (sel))

--tag @: ["id" @= id, hasClass cls] // "a"

-- could I do a string thing?
-- sure!
-- "#something"
-- ".something"
-- parse it good!

--scrapeSource :: Source -> IO (Maybe [Entry])
--scrapeSource (Source url chsel) = scrapeURL url wormEntries
  --where sel = chapterSelector chsel

--woot :: Scraper String [Entry]
--woot = chroots ("select" @: [] // "option") test
  --where
    --test = return $ Entry "woot" "woot"

-- they don't close their freaking option tags! Jerks!
-- well, it HAS to be either REGEX or post based
-- best option: RSS feed
    -- twig, pact, worm
-- next best option: parse links in text with REGEX
    -- twig pact fanfiction
-- hardest option: crawl the site, limit by url prefix
    -- all?
asdf = "<body><div> <a>woot</a> <select id='chap_select'><option  value=1 selected>1. Different Priors</option><option  value=2 >2. Pareidolia</option><option  value=3 >3. The Halo Effect</option><option  value=4 >4. Untested Solutions</option><option  value=5 >5. Dis-</option><option  value=6 >6. Garbage In, Garbage Out</option><option  value=7 >7. Tool Use</option><option  value=8 >8. Cult-Like Behavior</option><option  value=9 >9. Radiocarbon Dating</option><option  value=10 >10. Escape Sequence</option><option  value=11 >11. Master-Slave Configuration</option></select></div></body>"

--testScrape = do
    --putStrLn "SCRAPE"
    --let (Just es) = scrapeStringLike asdf woot
    ----(Just es) <- scrapeStringLike asdf woot
    --mapM print es
    --print "SCRAPE DONE"


