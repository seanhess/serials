{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Prelude
import Types
import Network.Wreq
import Control.Lens hiding (noneOf)
import Text.ParserCombinators.Parsec hiding (Parser, (<|>))
import Text.Parsec.ByteString.Lazy
import Data.ByteString.Lazy hiding (putStrLn)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS

import Control.Applicative hiding (many)


import Data.Word

ginnyURL = "http://fanfiction.net/s/11117811/"

testParse :: IO ()
testParse = do
    putStrLn "WOOT"
    fanfictionLinks ginnyURL
    putStrLn "Done"

fanfictionLinks :: String -> IO ()
fanfictionLinks url = do
    r <- get url
    let body = r ^. responseBody :: ByteString
    print body
    return ()

data Fragment = Option Int String | Unclassified String deriving (Show)

-- catch all...
--unclassified :: Parser Fragment
--unclassified = Unclassified <$> many letter_ascii

unclassified :: Parser Fragment
unclassified = do
    stuff <- many1 anyChar
    return $ Unclassified stuff

--webpage :: Parser [Fragment]
--webpage = (fragment `sepBy1` (string "option")) <* endOfInput

fragment :: Parser Fragment
fragment = optionTag <|> unclassified

optionTag :: Parser Fragment
optionTag = do
    string "<option"
    manyTill anyChar (string "value=")
    n <- many1 digit
    manyTill anyChar (char '>')
    chapterPrefix
    text <- many1 (noneOf "<>")
    return $ Option (read n) text
  where
    chapterPrefix = many digit >> char '.' >> many space

notOptionTag = do
    many anyChar
    notFollowedBy (string "<option")

--testParserNotWorking :: IO ()
--testParserNotWorking = print $ parseOnly webpage testString

testString = "<html> blash <select id=something title=\"whatever\"><option value=1 selected>1. First<option value=2>2. Second</select> farlkb <span> henry <li> </html"

testString2 = "asdf <option value=12 selected>1. First<option value=2>2. Second</select>"

run = parse (sepBy optionTag notOptionTag) "test" testString2
run' = parse (num `sepBy` many (char ' ')) "test" "234 34 234"

num :: Parser Int
num = read <$> many digit

--------------------------------------------------------------

everything :: Parser [Fragment]
everything = do
    string "<select"
    x <- many1 anyChar
    string "select"
    return $ [Option 4 x]

solution :: Parser [Fragment]
solution = everything <|> (anyChar >> solution)




--option :: Parser Fragment
--option =  

-- I'm parsing it... 
-- how? 
-- split on "<option "
-- eh... not split. Find the sub-matches
-- find <select, discard the other pieces
-- split on <option
-- just pull the pieces out
-- then 

------------------------------------------------------------
-- tutorial

--data IP = IP Word8 Word8 Word8 Word8 deriving Show

--parseIP :: Parser IP
--parseIP = do
    --d1 <- digit
    --char '.'
    --d2 <- digit
    --char '.'
    --d3 <- digit
    --char '.'
    --d4 <- digit
    --return $ IP d1 d2 d3 d4

--run :: IO ()
--run = print $ parseOnly (parseIP <* endOfInput) "134.234.423.34"

----------------------------------------------------------------

