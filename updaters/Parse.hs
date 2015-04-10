{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Prelude
import Types
import Network.Wreq hiding (Link)
import Data.Monoid ((<>))
import Data.Char (isLetter)
import Control.Lens hiding (noneOf)
import Data.Maybe (catMaybes)
import Text.ParserCombinators.Parsec hiding (Parser, (<|>))
import Text.Parsec.ByteString.Lazy
--import Data.ByteString.Lazy hiding (putStrLn)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS
import Text.HTML.TagSoup
import Data.ByteString.Lazy.UTF8 (toString)

import Control.Applicative hiding (many)

type URL = String

ginnyURL = "http://fanfiction.net/s/11117811/"

testParse :: IO ()
testParse = do
    putStrLn "WOOT"
    fanfictionLinks ginnyURL
    putStrLn "Done"

fanfictionLinks :: String -> IO [Link]
fanfictionLinks url = do
    r <- get url
    let body = r ^. responseBody :: BL.ByteString
    return $ fanficPageLinks url body

data FanficOption = FanficOption Int String
                  deriving Show

--------------------------------------------------------------
-- tags to links

fanficPageLinks :: URL -> BL.ByteString -> [Link]
fanficPageLinks base bs = map (optionToLink base) $ allOptions bs

optionToLink :: URL -> FanficOption -> Link
optionToLink base (FanficOption n t) = Link (base <> (show n)) t



--------------------------------------------------------------
-- parse tags

allOptions :: BL.ByteString -> [FanficOption]
allOptions = fanficOptionsFromTags . optionTags . parseTags

type BTag = Tag BL.ByteString

anyOpen :: BTag
anyOpen = TagOpen "" []

anyClose :: BTag
anyClose = TagClose ""

takeOptionTag :: [BTag] -> ([BTag], [BTag])
takeOptionTag [] = ([], [])
takeOptionTag (t:ts) = (option, rest)
  where
    option = t : takeWhile (not . isTagChange) ts
    rest   = dropWhile (not . isTagChange) ts

nextOptionTag :: [BTag] -> ([BTag], [BTag])
nextOptionTag = takeOptionTag . dropWhile (not . isOptionTag)

isTagChange :: BTag -> Bool
isTagChange t = t ~== anyOpen || t ~== anyClose

isOptionTag :: BTag -> Bool
isOptionTag t = t ~== open
  where
    open = TagOpen "option" [] :: Tag BL.ByteString

chunks :: ([a] -> ([a], [a])) -> [a] -> [[a]]
chunks f [] = []
chunks f xs = case c of
  [] -> cs
  ys -> ys : cs
  where
    (c, rest) = f xs
    cs = chunks f rest

optionTags = chunks nextOptionTag
fanficOptionsFromTags = catMaybes . map tagsToOption


tagsToOption :: [BTag] -> Maybe FanficOption
tagsToOption (TagOpen _ as : TagText text : []) = do
    ns <- lookup "value" as
    let t = toString text
        n = read $ toString ns
    return $ FanficOption n t

  where
    dropPrefix = dropWhile (not . isLetter)

tagsToOption _ = Nothing



