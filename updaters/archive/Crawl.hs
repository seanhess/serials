module Crawl where

import Control.Concurrent
import System.IO
import Text.HTML.Scalpel
import Data.Maybe (fromMaybe)
import Data.Map.Strict (Map)
import Data.List (isPrefixOf, nub, stripPrefix)
import qualified Data.Map.Strict as M
import Data.Monoid ((<>))
import Control.Concurrent.Chan
import Network.URI
import Data.Maybe (catMaybes, fromJust)
import Control.Applicative



type Title = String
type Url = String

type FoundLinks = Map Url Link

data Link = Link {
  linkURI :: URI,
  linkTitle :: Title
} deriving (Show, Eq)

linkPath :: Link -> String
linkPath = uriPath . linkURI

parseLinks :: Scraper String [Maybe Link]
parseLinks = chroots "a" parseLink

-- hmm... Or I should parse a button ... 
parseLink :: Scraper String (Maybe Link)
parseLink = do
  url <- attr "href" Any
  txt <- text Any
  let uri = parseURI url
  return $ Link <$> uri <*> Just txt

-- TODO add actual errors if something goes wrong
scrapePage :: String -> IO [Link]
scrapePage url = do
  mlinks <- scrapeURL url parseLinks
  return $ catMaybes $ fromMaybe [] $ mlinks

testCrawl :: IO ()
testCrawl = do
  putStrLn "TEST CRAWL"
  putStrLn "-------------------"

  let uri = fromJust $ parseURI "https://www.fanfiction.net/s/11117811/"

  found <- newMVar (M.fromList [(uriPath uri, Link uri "")])

  linksChan <- newChan
  forkIO $ crawler linksChan found uri
  writeChan linksChan uri
  getLine

  putStrLn "Stopping... found: "
  f <- takeMVar found
  print $ M.keys f



isUnvisited :: URI -> FoundLinks -> Bool
isUnvisited uri found = not $ M.member (uriPath uri) found

-- this won't work. needs to be an actual prefix
-- hmm... 
isValidSubLink :: URI -> URI -> Bool
isValidSubLink uri base = uriDomain uri == uriDomain base && uriPath base `isPrefixOf` uriPath uri

uriDomain :: URI -> String
uriDomain uri = case uriAuthority uri of
  Nothing   -> ""
  Just auth -> uriRegName auth


validLink :: FoundLinks -> URI -> Link -> Bool
validLink found base (Link uri title) = isUnvisited uri found && isValidSubLink uri base

crawler :: Chan URI -> MVar FoundLinks -> URI -> IO ()
crawler chan found base = loop
  where
    loop = do
      putStrLn "--------------"
      url <- readChan chan
      putStrLn ("Crawl: " <> show url)

      -- doesn't matter if it is VISISTED or not. It matters if it's already been queued
      -- write the new url into visited
      -- f <- takeMVar found
      -- print (M.keys f)
      -- putMVar f $ M.insert url True vs

      -- scrape it!
      rs <- scrapePage $ show url
      putStrLn $ "Found Links: " <> (show $ length rs)

      -- put valid links back on to the queue
      -- wait, it's not links that are VISITED, it's links that are QUEUED


      -- IDENTIFY: links that haven't been found
      f <- takeMVar found
      let nextLinks = nub $ filter (validLink f base) rs
      putStrLn $ "NEW Links: " <> (show $ length nextLinks)

      -- put them all on found
      -- reduce it baby!
      let f' = M.fromList $ map (\l -> (linkPath l, l)) nextLinks
      putMVar found $ M.union f f'

      putStrLn $ "FOUND: " <> ((show . M.keys) f')

      mapM_ (writeChan chan . linkURI) nextLinks

      loop
      return ()

-- this whole method won't work... 
-- because there aren't links on fanfiction!
