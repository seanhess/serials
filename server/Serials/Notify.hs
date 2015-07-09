{-# LANGUAGE OverloadedStrings #-}

-- notify users of new chapters!
module Serials.Notify where

import Prelude hiding (div)

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader

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

import Serials.Lib.Mail (Email(..), sendMail)
import Serials.Link.Link (contentText, Content)

import Text.Blaze.Html5 hiding (style, map)
import Text.Blaze.Html5.Attributes

import Serials.Types

-- notify people of all the new chapters
notifyChapters :: Source -> [Chapter] -> App ()
notifyChapters s cs = do
    us <- usersSubscribed (Source.id s)
    ep <- asks (endpoint . env)
    let es = map (notifyEmail ep s) cs
    mapM_ (notifyUsers us) es

notifyUsers :: [User] -> Email -> App ()
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




