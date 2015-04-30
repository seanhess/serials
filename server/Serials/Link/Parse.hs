{-# LANGUAGE OverloadedStrings #-}

module Serials.Link.Parse where

import Prelude
import Serials.Link.Link

import Data.Monoid ((<>))
import Data.Char (isLetter)
import Data.Maybe (catMaybes, fromJust)
import Data.Text (Text, pack)
import Data.Text.Read (decimal)

import qualified Data.Text as T
import Debug.Trace

import Text.ParserCombinators.Parsec hiding (Parser, (<|>))
import Text.HTML.TagSoup hiding (Tag)
import qualified Text.HTML.TagSoup as TagSoup

import Control.Applicative hiding (many)


data HTMLOption = HTMLOption Int Text
                  deriving Show

--------------------------------------------------------------
-- tags to links

type Tag = TagSoup.Tag Text

type TagSelect = [Tag] -> [Tag]
type TagMatch = Tag
type TagPred = Tag -> Bool

parseMenuContent :: URL -> TagSelect -> [Tag] -> [Content]
parseMenuContent base select ts = map (optionToContent base) $ allOptions $ select ts

optionToContent :: URL -> HTMLOption -> Content
optionToContent base (HTMLOption n t) = cleanLink (base <> (pack $ show n)) t

-------------------------------------------------------------
-- find the select tag

selectMenu :: TagMatch -> TagMatch -> ([Tag] -> [Tag])
selectMenu start end = takeWhile (~/= end) . dropWhile (~/= start)

selector :: Text -> TagMatch
selector = sel . css
  where
    sel (ID id)     = TagOpen "" [("id", id)]
    sel (Class cls) = TagOpen "" [("class", cls)]
    sel (Tag tag)   = TagOpen tag []

close :: Text -> TagMatch
close name = TagClose name

-------------------------------------------------------------


anyOpen :: Tag
anyOpen = TagOpen "" []

anyClose :: Tag
anyClose = TagClose ""

isTagChange :: Tag -> Bool
isTagChange t = t ~== anyOpen || t ~== anyClose

-------------------------------------------------------------
-- parsing <select> tags

allOptions :: [Tag] -> [HTMLOption]
allOptions = optionsFromTags . optionTags

takeOptionTag :: [Tag] -> ([Tag], [Tag])
takeOptionTag [] = ([], [])
takeOptionTag (t:ts) = (t : taken, rest)
  where
  (taken, rest) = span (not . isTagChange) ts

nextOptionTag :: [Tag] -> ([Tag], [Tag])
nextOptionTag = takeOptionTag . dropWhile (not . isOptionTag)

isOptionTag :: Tag -> Bool
isOptionTag = (~== open)
  where
    open = TagOpen "option" [] :: Tag

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


tagsToOption :: [Tag] -> Maybe HTMLOption
tagsToOption (TagOpen _ as : TagText text : []) = do
    ns <- lookup "value" as
    let t = text
    case decimal ns of
      Left err -> Nothing
      Right (n, _) -> return $ HTMLOption n t

  where
    dropPrefix = dropWhile (not . isLetter)

tagsToOption _ = Nothing




