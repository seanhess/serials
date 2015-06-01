{-# LANGUAGE OverloadedStrings #-}

module Serials.Lib.Mail (
  sendWelcomeEmail,
  sendInviteEmail,
  sendMail,
  Email(..)
) where

import System.Environment
import Data.Monoid ((<>))
import Data.ByteString (ByteString)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Maybe (fromJust)
import Network.API.Mandrill
import Text.Email.Validate (emailAddress)
import Text.Blaze (textValue)
import Text.Blaze.Html (Html, toHtml, toMarkup)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Serials.Model.User (User)
import qualified Serials.Model.User as U
import Serials.Model.Invite (Invite)
import qualified Serials.Model.Invite as I

import Serials.Model.App (readAllEnv, Env(..))

data Email = Email {
  emailSubject :: Text,
  emailBody :: Html
}

sendWelcomeEmail :: User -> IO ()
sendWelcomeEmail u = sendMail [U.email u] (welcomeEmail u)

sendInviteEmail :: Invite -> IO ()
sendInviteEmail i = do
  env <- readAllEnv
  sendMail [I.email i] (inviteEmail i (endpoint env))

sendMail :: [Text] -> Email -> IO ()
sendMail to (Email subj msg) = do
  env <- readAllEnv
  runMandrill (mandrill env) $ do
    let from = fromJust $ emailAddress "serials@orbit.al"
        tos  = map (fromJust . emailAddress . encodeUtf8) to
    res <- sendEmail (newHtmlMessage from tos subj msg)
    case res of
      MandrillSuccess k -> liftIO (print k)
      MandrillFailure f -> liftIO (print f)

welcomeEmail :: User -> Email
welcomeEmail u = Email "Welcome to Serials" $ do
  H.h3 ("Hello " >> toHtml (U.firstName u) >> ",")
  H.p "Welcome to Serials. Serials aims to be a podcast-like experience for reading serial publications on the web."
  H.p "Many excellent books and webcomics are being published openly on the web today. Most of those are published one chapter at a time. It's awesome that authors are self-publishing, but this format can make it hard to keep track of your progress."
  H.p "Serials lets you subscribe to your favorite works, keep track of your progress, and receive notifications when new content is published"
  H.hr
  H.p $ do
    "Have a suggestion? Want to be notified when we launch? Email us at "
    H.a H.! A.href "mailto:serials@orbit.al" $ "serials@orbit.al"
  H.hr

inviteEmail :: Invite -> Text -> Email
inviteEmail i endpoint = Email "Invite to join Serials" $ do
  H.h3 "You have been invited to join Serials,"
  H.p $ do
    "Just need to click this link and finishing creating your account: "
    H.a H.! A.href (textValue $ endpoint <> "/#/signup/" <> I.code i) $ "create account"

