{-# LANGUAGE OverloadedStrings #-}

module Serials.Read.Test where

import Prelude hiding (writeFile)

import Control.Applicative

import Debug.Trace
import Data.Text (Text, splitOn, intersperse, intercalate, unpack, pack)
import Data.Text.IO (writeFile)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Lazy (fromStrict)
import Data.Monoid

import Network.Wai
import Network.HTTP.Types
import Network.URI

import Serials.Link.Import
import Serials.Link.Link

import Text.HTML.TagSoup
import Text.Regex.PCRE


data Page = Page {
  pageHead :: [Tag Text],
  pageBody :: [Tag Text]
} deriving (Show, Eq)

-- download a page
-- replace all links
-- output it

proxyURL :: Text -> IO Text
proxyURL base = do
    body <- downloadBody base
    let tags = parseTags body
        out = renderTags $ map (fixTagURLs base) tags
    return out

--baseTag :: Text
--baseTag = "<base target=\"_parent\" />"

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

-- I want to be able to add a new one in the case of href
fixTagURLs :: Text -> Tag Text -> Tag Text
fixTagURLs base (TagOpen name as) = TagOpen name $ concat $ map fixAttURLs as
  where
    fixAttURLs ("href", url) = [("href", urlFromBase base url), ("target", "_parent")]
    fixAttURLs ("src",  url) = [("src",  urlFromBase base url)]
    fixAttURLs ("action",  url) = [("action", urlFromBase base url)]

    fixAttURLs ("onClick",  action) = [("onClick", replaceFanficLocation base action)]
    fixAttURLs ("onChange",  action) = [("onChange", replaceFanficLocation base action)]

    fixAttURLs att = [att]

fixTagURLs _ tag = tag

--"self.location='/woot'" =~ "^self.location='/w(.*)" :: [[String]]
replaceFanficLocation :: Text -> Text -> Text
replaceFanficLocation base action =
  case matches of
    ((start:url:[]):[]) -> "self.parent.location='" <> pack ("http://" <> domain <> url)
    _                   -> action

  where

  (Just domain) = do
    uri <- parseURIReference $ unpack $ base
    auth <- uriAuthority uri
    return $ uriRegName auth

  matches = unpack action =~ regex :: [[String]]
  regex = "^self.location\\s*=\\s*'(.*)" :: String

-- combine the urls, but don't parse the stuff after "?"
-- just throw it back on there
urlFromBase :: Text -> Text -> Text
urlFromBase base url = intercalate "?" ((base </> path) : rest)
  where
  (path:rest) = splitOn "?" url

proxyApp :: Application
proxyApp req respond = do
  let paths = pathInfo req
      url  = head paths
  out <- proxyURL url
  respond $ responseLBS status200 [("Content-Type", "text/html")] $ encodeUtf8 $ fromStrict out

--proxyChapter :: Text -> Application
--proxyChapter id req res = do
  --let mc = Chapter.find h id



