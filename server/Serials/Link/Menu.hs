{-# LANGUAGE OverloadedStrings #-}

module Serials.Link.Menu where

import Prelude

import Data.Monoid ((<>))
import Data.Char (isLetter)
import Data.Maybe (catMaybes, fromJust)
import Data.Text (Text, pack)
import Data.Text.Read (decimal)

import qualified Data.Text as T
import Debug.Trace

import Serials.Link.Link
import Serials.Link.Soup

import Text.ParserCombinators.Parsec hiding (Parser, (<|>))
import Text.HTML.TagSoup hiding (Tag)
import qualified Text.HTML.TagSoup as TagSoup



import Control.Applicative hiding (many)


data HTMLOption = HTMLOption Int Text
                  deriving Show

--------------------------------------------------------------
-- tags to links

parseMenuContent :: URL -> CSSSelector -> CSSSelector -> [Tag] -> [Content]
parseMenuContent base start end = map (optionToContent base) . matchersChunks [optionMatcher] . selectIn start end

optionToContent :: URL -> HTMLOption -> Content
optionToContent base (HTMLOption n t) = cleanLink (base <> (pack $ show n)) t

blockOption :: [Tag] -> Maybe HTMLOption
blockOption (TagOpen _ as : TagText text : []) = do
    ns <- lookup "value" as
    let t = text
    case decimal ns of
      Left err -> Nothing
      Right (n, _) -> return $ HTMLOption n t
  --where
    --dropPrefix = dropWhile (not . isLetter)

tagsToOption _ = Nothing

optionMatcher :: TagMatcher HTMLOption
optionMatcher = TagMatcher start end blockOption
  where start = match [(~== (open "option"))] 
        end   = match [anyChange]


--blockAnchor :: [Tag] -> Maybe HTMLContent
--blockAnchor (t:ts) = do
    --href <- maybeAttr "href" t
    --let txt = allText ts
    --guard (length txt > 0)
    --return $ HTMLAnchor href txt

--blockTitle :: [Tag] -> Maybe HTMLContent
--blockTitle ts = do
    --let txt = allText ts
    --guard (length txt > 0)
    --return $ HTMLTitle txt


-------------------------------------------------------------


  --tMatcher :: Maybe (TagMatcher HTMLContent)
  --tMatcher = titleMatcher <$> (matchSelector <$> mTitleSelector)

-------------------------------------------------------------
-- find the select tag

--selectMenu :: TagMatch -> TagMatch -> ([Tag] -> [Tag])
--selectMenu start end = takeWhile (~/= end) . dropWhile (~/= start)

--selector :: Text -> TagMatch
--selector = sel . css
  --where
    --sel (ID id)     = TagOpen "" [("id", id)]
    --sel (Class cls) = TagOpen "" [("class", cls)]
    --sel (Tag tag)   = TagOpen tag []

--close :: Text -> TagMatch
--close name = TagClose name

-------------------------------------------------------------

--anyOpen :: Tag
--anyOpen = TagOpen "" []

--anyClose :: Tag
--anyClose = TagClose ""

--isTagChange :: Tag -> Bool
--isTagChange t = t ~== anyOpen || t ~== anyClose

-------------------------------------------------------------
-- parsing <select> tags

--allOptions :: [Tag] -> [HTMLOption]
--allOptions = optionsFromTags . optionTags

--takeOptionTag :: [Tag] -> ([Tag], [Tag])
--takeOptionTag [] = ([], [])
--takeOptionTag (t:ts) = (t : taken, rest)
  --where
  --(taken, rest) = span (not . isTagChange) ts

--nextOptionTag :: [Tag] -> ([Tag], [Tag])
--nextOptionTag = takeOptionTag . dropWhile (not . isOptionTag)

--isOptionTag :: Tag -> Bool
--isOptionTag = (~== open)
  --where
    --open = TagOpen "option" [] :: Tag

--chunks :: ([a] -> ([a], [a])) -> [a] -> [[a]]
--chunks f [] = []
--chunks f xs = case c of
  --[] -> cs
  --ys -> ys : cs
  --where
    --(c, rest) = f xs
    --cs = chunks f rest

--optionTags = chunks nextOptionTag
--optionsFromTags = catMaybes . map tagsToOption





