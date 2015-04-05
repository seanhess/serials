module Crawl where

import Control.Concurrent
import System.IO
import Text.HTML.Scalpel
import Data.Maybe (fromMaybe)
import Data.Map.Strict (Map)
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as M
import Data.Monoid ((<>))
import Control.Concurrent.Chan


type Title = String
type Url = String
data Link = Link Url Title deriving (Show, Eq)

parseLinks :: Scraper String [Link]
parseLinks = chroots "a" parseLink

parseLink :: Scraper String Link
parseLink = do
  url <- attr "href" Any
  txt <- text Any
  return $ Link url txt

scrapePage :: String -> IO [Link]
scrapePage url = do
  print url
  mlinks <- scrapeURL url parseLinks
  return $ fromMaybe [] mlinks


testCrawl :: IO ()
testCrawl = do
  putStrLn "TEST CRAWL"
  putStrLn "-------------------"

  linksChan <- newChan
  forkIO $ crawler linksChan
  writeChan linksChan "https://parahumans.wordpress.com/"
  getLine
  putStrLn "Stopping"

  --q <- newEmptyMVar
  --forkIO $ crawler q
  --putStrLn "CRAWL"
  --us <- scrapePage "https://parahumans.wordpress.com/"
  --mapM_ print us
  --putStrLn "done"

isUnvisited :: Map Url Bool -> Url -> Bool
isUnvisited visited url = undefined

isValidSubLink :: Url -> Url -> Bool
isValidSubLink prefix url = prefix `isPrefixOf` url

-- hmm... no, that doesn't make too much sense
-- it would have to be ONE at a time, if that's what you want to do
-- or make a full channel and do that

crawler :: Chan Url -> IO ()
crawler q = loop
  where
    loop = do
      putStrLn "Crawl: waiting"
      url <- readChan q
      putStrLn ("Crawl: " <> url)
      rs <- scrapePage url
      print rs
      loop
      return ()

      --case cmd of
        --Message msg -> do
          --putStrLn msg
          --loop
        --Stop s -> do
          --putStrLn "logger: stop"
          --putMVar s ()

nextUrl :: MVar Url -> IO (Url)
nextUrl q = do
  url <- takeMVar q
  return url
  --putMVar q $ tail urls
  --return $ head urls

{-

DESIGN

Links: [(url, visited)]

- the function that adds a url can check for dups, and screen for matching the prefix

Is that an ... mvar? 

OK! What do I need now?

Need an MVar.. 

visited links: (url: true)

scrape a page, add to visited links
each link:
  add to queue if not visited

take link off the queue
find one that isn't visited? no...

-}
