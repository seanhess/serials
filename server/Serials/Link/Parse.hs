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
import Text.HTML.TagSoup

import Control.Applicative hiding (many)


data HTMLOption = HTMLOption Int Text
                  deriving Show

--------------------------------------------------------------
-- tags to links

type TagSelect = [BTag] -> [BTag]
type TagSelector = BTag

parseMenuLinks :: URL -> TagSelect -> [BTag] -> [Link]
parseMenuLinks base select ts = map (optionToLink base) $ allOptions $ select ts

optionToLink :: URL -> HTMLOption -> Link
optionToLink base (HTMLOption n t) = link (base <> (pack $ show n)) t

-------------------------------------------------------------
-- find the select tag

selectMenu :: TagSelector -> TagSelector -> ([BTag] -> [BTag])
selectMenu start end = takeWhile (~/= end) . dropWhile (~/= start)

closeSelector :: Text -> TagSelector
closeSelector t = TagClose t

openSelector :: Text -> TagSelector
openSelector = sel . parseSelector
  where
    sel (ID id)     = TagOpen "" [("id", id)]
    sel (Class cls) = TagOpen "" [("class", cls)]
    sel (Tag tag)   = TagOpen tag []

-------------------------------------------------------------

type BTag = Tag Text

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
takeOptionTag (t:ts) = (t : taken, rest)
  where
  (taken, rest) = span (not . isTagChange) ts

nextOptionTag :: [BTag] -> ([BTag], [BTag])
nextOptionTag = takeOptionTag . dropWhile (not . isOptionTag)

isOptionTag :: BTag -> Bool
isOptionTag = (~== open)
  where
    open = TagOpen "option" [] :: Tag Text

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
    let t = text
    case decimal ns of
      Left err -> Nothing
      Right (n, _) -> return $ HTMLOption n t

  where
    dropPrefix = dropWhile (not . isLetter)

tagsToOption _ = Nothing




