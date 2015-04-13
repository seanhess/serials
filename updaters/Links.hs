module Links where

import Text.HTML.Scalpel hiding (URL)
import qualified Data.ByteString.Lazy as BL
import Network.Wreq hiding (Link)
import Control.Lens hiding (noneOf)

type Title = String
type DateString = String
type URL = String

data Link = Link {
  linkURL :: URL,
  linkTitle :: Title
} deriving (Show, Eq)

downloadBody :: String -> IO BL.ByteString
downloadBody url = do
    r <- get url
    let body = r ^. responseBody :: BL.ByteString
    return body
