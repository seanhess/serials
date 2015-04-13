{-# LANGUAGE OverloadedStrings #-}
module Links where

import Prelude hiding (dropWhile)

import Data.Text
import Data.Text.Lazy (toStrict, fromStrict)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe, fromJust)
import Data.ByteString.Lazy (ByteString)

import Text.HTML.Scalpel hiding (URL)
import Network.Wreq hiding (Link)
import Control.Lens ((^.))

type Title = Text
type DateString = Text
type URL = Text

data Link = Link {
  linkURL :: URL,
  linkTitle :: Title
} deriving (Show, Eq)

downloadBody :: URL -> IO ByteString
downloadBody url = do
    r <- get (unpack url)
    let body = r ^. responseBody :: ByteString
    return body

(</>) :: URL -> URL -> URL
(</>) base path = (dropWhileEnd (=='/') base) <> "/" <> dropWhile (=='/') path
infixr 6 </>

decodeBS :: ByteString -> Text
decodeBS = toStrict . decodeUtf8

encodeBS :: Text -> ByteString
encodeBS = encodeUtf8 . fromStrict

data CSSSelector = ID Text | Class Text | Tag Text deriving (Show, Eq)

parseSelector :: Text -> CSSSelector
parseSelector xs = (sel . fromJust . uncons) xs
  where
    sel ('#',id) = ID id
    sel ('.',cls) = Class cls
    sel _   = Tag xs

-- if base has a trailing slash, then drop it
-- what if base is empty?
-- I don't really want to FAIL... I'd love to pattern match instead
