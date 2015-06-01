{-# LANGUAGE OverloadedStrings #-}

-- notify users of new chapters!
module Serials.Notify where

import Prelude hiding (div)

import Control.Monad

import Data.Pool (Pool)
import Data.Monoid ((<>))
import Data.Text (Text)

import Database.RethinkDB.NoClash (RethinkDBHandle)

import Serials.Model.Subscription (usersSubscribed)
import qualified Serials.Model.User as User

import Serials.Model.Chapter (Chapter)
import qualified Serials.Model.Chapter as Chapter
import Serials.Model.Source (Source)
import qualified Serials.Model.Source as Source
import Serials.Model.User (User)
import qualified Serials.Model.User as User
import Serials.Model.App (readAllEnv, Env(..))

import Serials.Lib.Mail (Email(..), sendMail)
import Serials.Link.Link (contentText, Content)

import Text.Blaze.Html5 hiding (style, map)
import Text.Blaze.Html5.Attributes

-- notify people of all the new chapters
notifyChapters :: Pool RethinkDBHandle -> Source -> [Chapter] -> IO ()
notifyChapters h s cs = do
    us <- usersSubscribed h (Source.id s)
    env <- readAllEnv
    let es = map (notifyEmail (endpoint env) s) cs
    mapM_ (notifyUsers us) es

notifyUsers :: [User] -> Email -> IO ()
notifyUsers us e = sendMail (map User.email us) e

-- this'll be the main email they get
notifyEmail :: Text -> Source -> Chapter -> Email
notifyEmail endpoint s c = Email subject body
  where
  chapterName = contentText (Chapter.content c)
  bookName = Source.name s

  subject =  chapterName <> " - " <> bookName

  body = do
    p $ "A new chapter was just published"
    p $ do
      i $ toHtml bookName
      ": "
      i $ toHtml chapterName
    p $ a ! href (textValue $ endpoint <> "/#/books/" <> Source.id s) $ "Click here to read"




