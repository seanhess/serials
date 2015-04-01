module Scrape ( 
  testScrape 
) where

import Text.HTML.Scalpel
import Control.Applicative

-- many of them have arcs

type Title = String
type Url = String

type Id = String
type Class = String

data Entry = Entry Title Url deriving (Show, Eq)
data Source = Source Url (Selector String)

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

worm = Source "https://parahumans.wordpress.com/" (selector "#categories-2" // "a")
pact = Source "https://pactwebserial.wordpress.com/table-of-contents/" (selector "#categories-2" // "a")
ginny = Source "https://www.fanfiction.net/s/11117811/" (selector "#chap_select")

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

selector :: String -> Selector String
selector ('#':id) = Any @: ["id" @= id]
selector ('.':cls) = Any @: [hasClass cls] 
selector tag = tag @: []

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

woot :: Scraper String [Entry]
woot = chroots ("select" @: [] // "option") test
  where
    test = return $ Entry "woot" "woot"

-- they don't close their freaking option tags! Jerks!
-- well, it HAS to be either REGEX or post based
-- best option: RSS feed
    -- twig, pact, worm
-- next best option: parse links in text with REGEX
    -- twig pact fanfiction
-- hardest option: crawl the site, limit by url prefix
    -- all?
asdf = "<body><div> <a>woot</a> <select id='chap_select'><option  value=1 selected>1. Different Priors</option><option  value=2 >2. Pareidolia</option><option  value=3 >3. The Halo Effect</option><option  value=4 >4. Untested Solutions</option><option  value=5 >5. Dis-</option><option  value=6 >6. Garbage In, Garbage Out</option><option  value=7 >7. Tool Use</option><option  value=8 >8. Cult-Like Behavior</option><option  value=9 >9. Radiocarbon Dating</option><option  value=10 >10. Escape Sequence</option><option  value=11 >11. Master-Slave Configuration</option></select></div></body>"

testScrape = do
    putStrLn "SCRAPE"
    let (Just es) = scrapeStringLike asdf woot
    --(Just es) <- scrapeStringLike asdf woot
    mapM print es
    print "SCRAPE DONE"


-- Easiest: parse one page. Works for fanfiction, nothing else will work well

-- goal of this: be able to add a book, specify an algorithm, and a url
-- return an accurate list of chapters, given an algorithm and a url


-- Worm
-- HPMOR
-- fanfiction
-- legion of nothing: http://inmydaydreams.com/

-- RSS feeds? HPMOR and Worm have them. Do others?
-- https://hackage.haskell.org/package/feed
-- https://hackage.haskell.org/package/scalpel
-- tag soup

-- try to parse an actual table of contents? No, it might not be up to date!


-- other competitors: https://www.comic-rocket.com/
-- used to be: serialist.com
-- still, that seems like the way to go. Can I crawl the whole page, and get urls?
-- hmm... I need to know the link text too

{-

HPMOR: rss
FANFICTION: toc-dropdown
WORM: toc-page.     site-map  rss
PACT: toc-page      site-map  rss
TWIG: toc-dropdown. site-map  rss
Legion: 

Can you reduce all of these to some root entry point? The SAME PAGE you can always parse to get new content?

Divide them into groups:

Wordpress (Worm, Twig, Pact)
FanFiction

Ooh, what about an algorithm that follows all links on the site and generates a sitemap?

1. hit the core page and parse
2. gather all links with link text
3. each link: if on the same site, recurse and merge results, otherwise ignore

What to call each one? the biggest title?
How to exclude non-chapters?

Use a chapter regex?

Maybe it WOULD be easier to just find a page we can parse on each one containing EVERYTHING

There's no good way to tell from a link if it is a chapter or something else?

Simplest method: 
  - parse a single page
  - parse some sub-node that helps
  - clean it up

-}
