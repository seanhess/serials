{-# LANGUAGE OverloadedStrings #-}

module Serials.Link.Menu where

import Prelude hiding (dropWhile)

import Data.Monoid ((<>))
import Data.Char (isLetter)
import Data.Maybe (catMaybes, fromJust)
import Data.Text (Text, pack, dropWhile)
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
    case decimal ns of
      Left err -> Nothing
      Right (n, _) -> return $ HTMLOption n text

  -- we can't drop the numbers. Some of them are just numbers!
  --where
    --dropPrefix = dropWhile (not . isLetter)

tagsToOption _ = Nothing

optionMatcher :: TagMatcher HTMLOption
optionMatcher = TagMatcher start end blockOption
  where start = match [(~== (open "option"))]
        end   = match [anyChange]


