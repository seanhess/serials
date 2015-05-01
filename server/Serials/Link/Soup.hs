{-# LANGUAGE OverloadedStrings #-}

module Serials.Link.Soup where

import Prelude hiding (null, length, readFile, writeFile)

import Debug.Trace

import Safe
import Data.Maybe
import Data.Text (Text, strip, null, length, uncons)
import Data.Monoid ((<>), mconcat)
import Data.List (unfoldr, find)

import Control.Applicative
import Control.Monad

import Text.HTML.TagSoup hiding (Tag)
import qualified Text.HTML.TagSoup as TagSoup

import Text.HTML.Scalpel hiding (select)
import qualified Text.HTML.Scalpel as Scalpel


-------------------------------------------------------

data CSSSelector = ID Text | Class Text | Tag Text deriving (Show, Eq)

selector :: Text -> Selector Text
selector = soupSelector . css

soupSelector :: CSSSelector -> Selector Text
soupSelector = sel
  where
    sel (ID id)     = Any @: [("id" :: Text) @= id]
    sel (Class cls) = Any @: [hasClass cls] 
    sel (Tag tag)   = tag @: []

select :: CSSSelector -> [Tag] -> [Tag]
select css = fromMaybe [] . headMay . Scalpel.select (soupSelector css)

css :: Text -> CSSSelector
css xs = (sel . fromJust . uncons) xs
  where
    sel ('#',id) = ID id
    sel ('.',cls) = Class cls
    sel _   = Tag xs

-------------------------------------------------------

-- start and end!
data TagMatcher = TagMatcher {
  isStart :: (Tag -> Bool),
  isEnd   :: (Tag -> Bool)
  -- then you can add something else, like a potential output here
}

-- skips and finds the one that matched
skipNextMatchers :: [TagMatcher] -> [Tag] -> (Maybe TagMatcher, [Tag])
skipNextMatchers ms [] = (Nothing, [])
skipNextMatchers ms (t:ts) = 
  case find (isMatch t) ms of
    Nothing -> skipNextMatchers ms ts
    Just m  -> (Just m, (t:ts))
  where isMatch t m = isStart m t

takeNextMatcher :: TagMatcher -> [Tag] -> ([Tag], [Tag])
takeNextMatcher m = takeNext (isStart m)

findNextMatchers :: [TagMatcher] -> [Tag] -> ([Tag], [Tag])
findNextMatchers ms ts = 
  case skipNextMatchers ms ts of
    (Nothing, _) -> ([], [])
    (Just m, ts') -> takeNextMatcher m ts'

matchersChunks :: [TagMatcher] -> [Tag] -> [[Tag]]
matchersChunks = chunks . findNextMatchers

--------------------------------------------------------

type Tag = TagSoup.Tag Text

allText :: [Tag] -> Text
allText = strip . innerText

maybeAttr :: Text -> Tag -> Maybe Text
maybeAttr name (TagOpen _ as) = lookup name as
maybeAttr _ _ = Nothing

-- change this to work with two tag chunks instead?
(<||>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(<||>) f g t = f t || g t

-- take next should always take at least one tag
takeNext :: (Tag -> Bool) -> [Tag] -> ([Tag], [Tag])
takeNext end (t:ts) = (t:tks, rest)
  where
  (tks, rest) = span (not . end) ts
takeNext _ _ = ([], [])

skipNext :: (Tag -> Bool) -> [Tag] -> [Tag]
skipNext start = dropWhile (not . start)

chunk :: (Tag -> Bool) -> (Tag -> Bool) -> [Tag] -> ([Tag], [Tag])
chunk start end = takeNext end . skipNext start

chunks :: ([a] -> ([a], [a])) -> [a] -> [[a]]
chunks f xs = unfoldr f' xs
  where f' [] = Nothing
        f' xs' = Just $ f xs'

isText :: Tag -> Bool
isText (TagText txt) = not . null . strip $ txt
isText _ = False

open :: Text -> Tag
open name = TagOpen name []

close :: Text -> Tag
close name = TagClose name

anyText :: Tag
anyText = TagText ""
