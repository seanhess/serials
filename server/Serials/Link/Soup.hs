{-# LANGUAGE OverloadedStrings #-}

module Serials.Link.Soup where

import Prelude hiding (null, length, readFile, writeFile)

import Debug.Trace

import Safe
import Data.Maybe
import Data.Text (Text, strip, null, length, uncons)
import Data.Monoid ((<>), mconcat)
import Data.List (unfoldr, find, tails)

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
data TagMatcher a = TagMatcher {
  isStart :: ([Tag] -> Bool),
  isEnd   :: ([Tag] -> Bool),
  toValue   :: ([Tag] -> Maybe a)
}

-- skips and finds the one that matched
skipNextMatchers :: [TagMatcher a] -> [Tag] -> (Maybe (TagMatcher a), [Tag])
skipNextMatchers ms [] = (Nothing, [])
skipNextMatchers ms ts = 
  case find (isMatch ts) ms of
    Nothing -> skipNextMatchers ms (tail ts)
    Just m  -> (Just m, ts)
  where isMatch ts m = isStart m ts

takeNextMatcher :: TagMatcher a -> [Tag] -> ([Tag], [Tag])
takeNextMatcher m = takeNext (isEnd m)

findNextMatchers :: [TagMatcher a] -> [Tag] -> (Maybe a, [Tag])
findNextMatchers ms ts = 
  case skipNextMatchers ms ts of
    (Nothing, _) -> (Nothing, [])
    (Just m, ts') -> 
      let (block, ts'') = takeNextMatcher m ts'
      in  (toValue m block, ts'')

matchersChunks :: [TagMatcher a] -> [Tag] -> [a]
matchersChunks ms ts = catMaybes $ chunks (findNextMatchers ms) ts

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

-- always take at least one item off the list
takeNext :: ([Tag] -> Bool) -> [Tag] -> ([Tag], [Tag])
takeNext end (t:ts) = (t:tks, rest)
  where
  (tks, rest) = spanTails (not . end) ts
takeNext _ _ = ([], [])

-- like span, but sends the predicate the whole list instead of just the first element
spanTails                    :: ([a] -> Bool) -> [a] -> ([a],[a])
spanTails _ xs@[]            =  (xs, xs)
spanTails p xs@(x:xs')
              | p (x:xs')    =  let (ys,zs) = spanTails p xs' in (x:ys,zs)
              | otherwise    =  ([],xs)


skipNext :: ([Tag] -> Bool) -> [Tag] -> [Tag]
skipNext start = fromMaybe [] . headMay . dropWhile (not . start) . tails

--chunk :: ([Tag] -> Bool) -> (Tag -> Bool) -> [Tag] -> ([Tag], [Tag])
--chunk start end = takeNext end . skipNext start

chunks :: ([a] -> (b, [a])) -> [a] -> [b]
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

match :: [(Tag -> Bool)] -> [Tag] -> Bool
match ms ts = and $ map (\(f, t) -> f t) $ zip ms ts

