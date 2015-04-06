
module UrlPatterns where

import Control.Concurrent
import Types
import Text.HTML.Scalpel
import Data.Monoid ((<>))
import Control.Concurrent.Chan
import Control.Applicative

-- for now, just append the number to it
-- just use scrapeURL!

findIncrementing :: URL -> IO [Link]
findIncrementing base = do

    -- how many to put on?

    let num = 1

    -- find channel
    cfind <- newChan
    writeChan cfind (base, num)

    -- results channel
    cdone <- newChan

    forkIO $ worker cfind cdone

    results <- collect [] cdone

    putStrLn "DONE"
    mapM_ print results
    return results

  where
    collect links cdone = do
      ml <- readChan cdone
      case ml of
        Nothing   -> return $ links
        Just link -> collect (link : links) cdone



findPageTitle :: URL -> IO (Maybe String)
findPageTitle url = scrapeURL url scrapeTitle

pageUrl :: URL -> Int -> URL
pageUrl base num = base <> show num

-- you don't need channels for this at all :)
worker :: Chan (URL, Int) -> Chan (Maybe Link) -> IO ()
worker next done = loop
  where 
    loop = do
      (base, num) <- readChan next
      let url = pageUrl base num
      putStrLn $ "FETCHING: " <> url

      mt <- findPageTitle url
      
      case mt of
        Nothing -> putStrLn ("Missed " <> show num)
        Just t  -> writeChan next (base, num+1)

      writeChan done $ Link <$> Just url <*> mt
      loop

scrapeTitle :: Scraper String String
scrapeTitle = text "title"

-- I'm just scraping for the title of the page, or the first <h1> or something

