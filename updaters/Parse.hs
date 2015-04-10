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

fanfictionLinks :: String -> IO ()
fanfictionLinks url = do
    r <- get url
    let body = r ^. responseBody :: BL.ByteString
    print body
    return ()

data FanficOption = FanficOption Int String
                  deriving Show

----------------------------------------------------------

--data Fragment = Option Int String | Unclassified String deriving (Show)

---- catch all...
----unclassified :: Parser Fragment
----unclassified = Unclassified <$> many letter_ascii

--unclassified :: Parser Fragment
--unclassified = do
    --stuff <- many1 anyChar
    --return $ Unclassified stuff

----webpage :: Parser [Fragment]
----webpage = (fragment `sepBy1` (string "option")) <* endOfInput

--fragment :: Parser Fragment
--fragment = optionTag <|> unclassified

--optionTag :: Parser Fragment
--optionTag = do
    --string "<option"
    --manyTill anyChar (string "value=")
    --n <- many1 digit
    --manyTill anyChar (char '>')
    --chapterPrefix
    --text <- many1 (noneOf "<>")
    --return $ Option (read n) text
  --where
    --chapterPrefix = many digit >> char '.' >> many space

--notOptionTag = do
    --many anyChar
    --notFollowedBy (string "<option")

--testParserNotWorking :: IO ()
--testParserNotWorking = print $ parseOnly webpage testString

--everything :: Parser [Fragment]
--everything = do
    --string "<select"
    --x <- many1 anyChar
    --string "select"
    --return $ [Option 4 x]

--solution :: Parser [Fragment]
--solution = everything <|> (anyChar >> solution)


---------------------------------------------------------------

testString :: BL.ByteString
testString = "<html> blash <select id=something title=\"whatever\"><option value=1 selected>1. First<option value=2>2. Second</select> farlkb <span> henry <li> </html"

testString2 = "asdf <option value=12 selected>1. First<option value=2>2. Second</select>"

--run = parse (sepBy optionTag notOptionTag) "test" testString2
--run' = parse (num `sepBy` many (char ' ')) "test" "234 34 234"

num :: Parser Int
num = read <$> many digit

--------------------------------------------------------------

-- parse one for fanfiction options!
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


--------------------------------------------------------------

optionToLink :: URL -> FanficOption -> Link
optionToLink base (FanficOption n t) = Link (base <> (show n)) t

fanficPageLinks :: URL -> BL.ByteString -> [Link]
fanficPageLinks base bs = map (optionToLink base) $ allOptions bs

---------------------------------------------------------------

