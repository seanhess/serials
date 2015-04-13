{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Prelude
import Links
import Data.Monoid ((<>))
import Data.Char (isLetter)
import Data.Maybe (catMaybes, fromJust)
import Data.Text (Text, pack)
import qualified Data.Text as T

import Text.ParserCombinators.Parsec hiding (Parser, (<|>))
import Text.Parsec.ByteString.Lazy
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS
import Text.HTML.TagSoup
import Data.ByteString.Lazy.UTF8 (toString, fromString)

import Control.Applicative hiding (many)

-- this works for: twig, and ginny
ginnyURL = "http://fanfiction.net/s/11117811/"
twigURL = "https://twigserial.wordpress.com/"

-- can I get it to work for link tocs too?
-- almost definitely :)

pactURL = "https://pactwebserial.wordpress.com/table-of-contents/"

testTwig = menuLinks "https://twigserial.wordpress.com/donate/" "https://twigserial.wordpress.com/?cat=" (openSelector "#cat") (closeSelector "select")

testGinny = menuLinks "http://fanfiction.net/s/11117811/" "http://fanfiction.net/s/11117811/" (openSelector "#chap_select") (closeSelector "select")

--fanfictionLinks :: URL -> IO [Link]
--fanfictionLinks url = menuLinks url url (openSelector "#chap_select") (closeSelector "select")

menuLinks :: URL -> URL -> TagSelector -> TagSelector -> IO [Link]
menuLinks url base start end = do
    body <- downloadBody url
    let tags = parseTags body
        select = selectMenu start end
    return $ parseMenuLinks base select tags

data HTMLOption = HTMLOption Int Text
                  deriving Show


--------------------------------------------------------------
-- tags to links

type TagSelect = [BTag] -> [BTag]
type TagSelector = BTag

parseMenuLinks :: URL -> TagSelect -> [BTag] -> [Link]
parseMenuLinks base select ts = map (optionToLink base) $ allOptions $ select ts

optionToLink :: URL -> HTMLOption -> Link
optionToLink base (HTMLOption n t) = Link (base <> (pack $ show n)) t

-------------------------------------------------------------
-- find the select tag

selectMenu :: TagSelector -> TagSelector -> ([BTag] -> [BTag])
selectMenu start end = takeWhile (~/= end) . dropWhile (~/= start)

closeSelector :: Text -> TagSelector
closeSelector t = TagClose (encodeBS t)

openSelector :: Text -> TagSelector
openSelector = sel . parseSelector
  where
    sel (ID id)     = TagOpen "" [("id", encodeBS id)]
    sel (Class cls) = TagOpen "" [("class", encodeBS cls)]
    sel (Tag tag)   = TagOpen (encodeBS tag) []

-------------------------------------------------------------

type BTag = Tag BL.ByteString

anyOpen :: BTag
anyOpen = TagOpen "" []

anyClose :: BTag
anyClose = TagClose ""

isTagChange :: BTag -> Bool
isTagChange t = t ~== anyOpen || t ~== anyClose

-------------------------------------------------------------
-- parsing <select> tags

allOptions :: [BTag] -> [HTMLOption]
allOptions = optionsFromTags . optionTags

takeOptionTag :: [BTag] -> ([BTag], [BTag])
takeOptionTag [] = ([], [])
takeOptionTag (t:ts) = (option, rest)
  where
    option = t : takeWhile (not . isTagChange) ts
    rest   = dropWhile (not . isTagChange) ts

nextOptionTag :: [BTag] -> ([BTag], [BTag])
nextOptionTag = takeOptionTag . dropWhile (not . isOptionTag)


isOptionTag :: BTag -> Bool
isOptionTag = (~== open)
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
optionsFromTags = catMaybes . map tagsToOption


tagsToOption :: [BTag] -> Maybe HTMLOption
tagsToOption (TagOpen _ as : TagText text : []) = do
    ns <- lookup "value" as
    let t = decodeBS text
        n = read $ toString ns
    return $ HTMLOption n t

  where
    dropPrefix = dropWhile (not . isLetter)

tagsToOption _ = Nothing




