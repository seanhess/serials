{-# LANGUAGE OverloadedStrings #-}

module Serials.Proxy.Test where

import Prelude hiding (writeFile)

import Debug.Trace
import Data.Text (Text)
import Data.Text.IO (writeFile)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Lazy (fromStrict)
import Data.Monoid

import Network.Wai
import Network.HTTP.Types

import Serials.Link.Import
import Serials.Link.Link

import Text.HTML.TagSoup


-- download a page
-- replace all links
-- output it

proxyURL :: Text -> IO Text
proxyURL base = do
    body <- downloadBody base
    let tags = parseTags body
        out = renderTags $ map (fixTagURLs base) tags
    --writeFile "/Users/seanhess/Downloads/test.html" out
    -- replace all href="/
    -- parse the uris and such yo
    --putStrLn "TEST"
    return out


--testFriendship = test "http://www.fimfiction.net/story/62074/2/friendship-is-optimal/1-opportunity"
--testHpmor = test "http://hpmor.com/chapter/21"
--testTwig = test "https://twigserial.wordpress.com/category/story/arc-1-taking-root/1-08/"
--testPact = test "https://pactwebserial.wordpress.com/2014/01/04/bonds-1-5/"
--testMother = test "https://www.fictionpress.com/s/2961893/3"

-- TODO
-- fanfiction uses onClick and other weirdness to compensate for
-- if I proxy I can hijack the next and prev links too!
-- then I could know they had finished reading if they hit next...
-- that sounds super fancy...

fixTagURLs :: Text -> Tag Text -> Tag Text
fixTagURLs base (TagOpen name as) = TagOpen name $ map fixAttURLs as
  where
    fixAttURLs ("href", url) = ("href", base </> url)
    fixAttURLs ("src",  url) = ("src",  base </> url)
    fixAttURLs ("action",  url) = ("action",  base </> url)
    -- fanfiction does some wierd stuff here
    --fixAttURLs ("onClick",  url) = ("onClick",  base </> url)
    fixAttURLs att = att

fixTagURLs _ tag = tag

proxyApp :: Application
proxyApp req respond = do
  let paths = pathInfo req
      url  = head paths
  out <- proxyURL url
  respond $ responseLBS status200 [("Content-Type", "text/html")] $ encodeUtf8 $ fromStrict out


