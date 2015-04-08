
module UrlPatterns where

import Control.Concurrent
import Types
import Text.HTML.Scalpel
import Data.Monoid ((<>))
import Control.Concurrent.Chan
import Control.Applicative
import Data.Maybe (isJust, catMaybes)
import Data.List (takeWhile)


-- find all valid links under a domain that follow the pattern:
-- http://example.com/pages/(1..N)
-- as soon as one is missing, return a list of all the ones you found

-- SOLUTIONS
-- mapConcurrently from async will work fine

findIncrementing :: URL -> IO [Link]
findIncrementing base = do
    find <- newChan
    done <- newChan
    forkIO $ worker find done
    queue find done base

queue :: Chan (URL, Int) -> Chan (Maybe Link) -> URL -> IO [Link]
queue find done base = next [] 1
  where
    next links n = do
      putStrLn $ "Next" <> show n
      writeChan find (base, n)
      ml <- readChan done
      case ml of
        Nothing -> return links
        Just l  -> do
          next (l:links) (n+1)

worker :: Chan (URL, Int) -> Chan (Maybe Link) -> IO ()
worker next done = loop
  where 
    loop = do
      (base, num) <- readChan next
      let url = pageUrl base num
      putStrLn $ "FETCHING: " <> url

      mt <- findPageTitle url

      writeChan done $ Link <$> Just url <*> mt
      loop

scrapeTitle :: Scraper String String
scrapeTitle = text "title"

findPageTitle :: URL -> IO (Maybe String)
findPageTitle url = scrapeURL url scrapeTitle

pageUrl :: URL -> Int -> URL
pageUrl base num = base <> show num


-- https://hackage.haskell.org/package/async-2.0.2/docs/Control-Concurrent-Async.html
