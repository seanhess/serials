
module UrlPatterns where

import Control.Concurrent
import Links
import Scrape (scrapeTitle)
import Text.HTML.Scalpel hiding (URL)
import Data.Monoid ((<>))
import Control.Concurrent.Chan
import Control.Applicative
import Data.Maybe (isJust, catMaybes)
import Data.List (takeWhile)
import Control.Monad.Loops (whileJust)


-- find all valid links under a domain that follow the pattern:
-- http://example.com/pages/(1..N)
-- as soon as one is missing, return a list of all the ones you found

-- SOLUTIONS
-- mapConcurrently from async will work fine

--writeChan find (base, n)

findIncrementing :: URL -> IO [Link]
findIncrementing base = do
    find <- newChan
    done <- newChan
    forkIO $ worker find done

    writeChan find (base, 1)
  
    whileJust (readChan done) $ \link -> do
      return link

worker :: Chan (URL, Int) -> Chan (Maybe Link) -> IO ()
worker next done = loop
  where 
    loop = do
      (base, num) <- readChan next
      let url = pageUrl base num
      putStrLn $ "FETCHING: " <> url

      mt <- findPageTitle url

      writeChan done $ Link <$> Just url <*> mt <*> Just ""

      case mt of
        Nothing -> return ()
        Just t  -> writeChan next (base, num+1)

      loop

findPageTitle :: URL -> IO (Maybe String)
findPageTitle url = scrapeURL url scrapeTitle

pageUrl :: URL -> Int -> URL
pageUrl base num = base <> show num


-- https://hackage.haskell.org/package/async-2.0.2/docs/Control-Concurrent-Async.html
