{-# LANGUAGE OverloadedStrings #-}

module Serials.Link.Test where

import Prelude hiding (null, length, readFile, writeFile)

import Debug.Trace

import Safe
import Data.Maybe
import Data.Text (Text, strip, null, length)
import Data.Text.IO (writeFile, readFile)
import qualified Data.Text.Lazy as TL
import Data.Monoid ((<>), mconcat)
import Data.List (unfoldr)

import Control.Lens (to, only, nearly, (^?), ix, toListOf, (^.), _Just, (^..), universe, traverse)
import Control.Applicative
import Control.Monad

import Serials.Link.Import
import Serials.Link.Link
--import Serials.Link.Parse
import Serials.Link.Scrape (soupSelector)


import Text.HTML.TagSoup hiding (Tag)
import qualified Text.HTML.TagSoup as TagSoup
import Text.HTML.Scalpel hiding (select)
import qualified Text.HTML.Scalpel as Scalpel

--import Text.XML.PolySoup
--import Text.HTML.Tagchup.Tag

-- PolySoup ---------------------------------------------------------


test = do
    body <- readFile "./dist/worm.html"
    --body <- downloadBody "https://parahumans.wordpress.com/table-of-contents/"
    --writeFile "./dist/worm.html" body

    let tags = parseTags body
        content = select (Class "entry-content") tags
    mapM_ print $ map blockContent $ chunks chunkTOC content

----------------------------------------------------------------------

-- good lord it's beautiful!

chunkTOC :: [Tag] -> ([Tag], [Tag])
chunkTOC = chunk start end
  where
  start = isOpenAnchor <||> isText
  end   = isOpenAnchor <||> (~== (close "a"))

blockContent :: [Tag] -> Maybe Content
blockContent [] = Nothing
blockContent (t:ts)
  | isTagText t = blockTitle (t:ts)
  | otherwise   = blockLink (t:ts)

blockLink :: [Tag] -> Maybe Content
blockLink (t:ts) = do
    href <- maybeAttr "href" t
    let txt = allText ts
    return $ Link href txt

blockTitle :: [Tag] -> Maybe Content
blockTitle ts = do
    let txt = allText ts
    guard (length txt > 0)
    return $ Title txt

-------------------------------------------------------------------

type Tag = TagSoup.Tag Text
data TagChunk = TagChunk (Tag -> Bool) (Tag -> Bool)

allText :: [Tag] -> Text
allText = strip . innerText

maybeAttr :: Text -> Tag -> Maybe Text
maybeAttr name (TagOpen _ as) = lookup name as
maybeAttr _ _ = Nothing

chunk :: (Tag -> Bool) -> (Tag -> Bool) -> [Tag] -> ([Tag], [Tag])
chunk start end = takeNext end . skipNext start

-- change this to work with two tag chunks instead?
(<||>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(<||>) f g t = f t || g t

-- take next should always take at least one tag
takeNext :: (Tag -> Bool) -> [Tag] -> ([Tag], [Tag])
takeNext end (t:ts) = (t:tks, rest)
  where
  (tks, rest) = span (not . end) ts
takeNext _ _ = ([], [])

chunks :: ([a] -> ([a], [a])) -> [a] -> [[a]]
chunks f xs = unfoldr f' xs
  where f' [] = Nothing
        f' xs' = Just $ f xs'

isOptionTag :: Tag -> Bool
isOptionTag = (~== open)
  where
    open = TagOpen "option" [] :: Tag

select :: CSSSelector -> [Tag] -> [Tag]
select css = fromMaybe [] . headMay . Scalpel.select (soupSelector css)

skipNext :: (Tag -> Bool) -> [Tag] -> [Tag]
skipNext start = dropWhile (not . start)


isText :: Tag -> Bool
isText (TagText txt) = not . null . strip $ txt
isText _ = False

isOpenAnchor :: Tag -> Bool
isOpenAnchor = (~== open "a")

open :: Text -> Tag
open name = TagOpen name []

close :: Text -> Tag
close name = TagClose name

anyText :: Tag
anyText = TagText ""

------------------------------------------------------------------------------



-- Taggy Lenses! ----------------------------------------------------
--import Text.Taggy
--import Text.Taggy.Lens

-- what's my algorithm here?
-- if you hit an "a", parse it
-- if you hit text not in an a, parse it
-- more like straight up tag soup anyway

--test = do
    --body <- downloadBodyLazy "https://parahumans.wordpress.com/table-of-contents/"
    --let tags = taggyWith True body
        --(NodeElement root) = head . domify $ tags
        --elements = (root /- [attrMatch "class" "entry-content"]) /* [flip hasName "a" ||| flip hasName "strong"]

    --let cs = map toContent elements

    ----putStrLn $ take 500 $ show $ umm
    --mapM_ print $ drop 30 $ take 50 $ zip cs elements
  
    --putStrLn "HI"

--attrMatch :: Text -> Text -> Element -> Bool
--attrMatch key m el = case getAttr el key of
  --Nothing  -> False
  --Just val -> val == m

--anything :: Element -> Bool
--anything _ = True

--(/-) :: Element -> [(Element -> Bool)] -> Element
--(/-) el ps = head $ el /* ps

--(|||) :: (Element -> Bool) -> (Element -> Bool) -> Element -> Bool
--(|||) one two el = one el || two el

----------------------------------------------------------------------------------

--taggyToContent :: Element -> Ma

-- not sure how to do an OR query against it. not easy, that's for sure :)
--parseTaggyLenses :: TL.Text -> [Maybe Content]
--parseTaggyLenses body = cs
  --where
    --tags = taggyWith True body
    --cs = body ^.. html . allAttributed (ix "class" . only "entry-content")
    --   . allNamed (nearly "what does this do?" (\n -> n == "a" || n == "strong")) . to toContent

-- needs to see what we can find
--toContent :: Element -> Content
--toContent el = makeContent href text
  --where
    --href = el ^. attr "href"
    --text = el ^. children . traverse . content
    --makeContent Nothing text = Title text
    --makeContent (Just href) text = Link href text

