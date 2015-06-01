{-# LANGUAGE OverloadedStrings #-}

module Serials.Lib.Mail (
  sendWelcomeEmail,
  sendInviteEmail,
  MailConfig(..)
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

data MailConfig = MailConfig {link :: Text}

defaultMailConfig :: MailConfig
defaultMailConfig = MailConfig { link = "http://localhost:3001" }

sendWelcomeEmail :: User -> IO ()
sendWelcomeEmail u = sendMail (encodeUtf8 $ U.email u) "Welcome to Serials" (welcomeEmail u)

sendInviteEmail :: Invite -> MailConfig -> IO ()
sendInviteEmail i c = do
  sendMail (encodeUtf8 $ I.email i) "Invite to join Serials" (inviteEmail i (link c))

sendMail :: ByteString -> Text -> Html -> IO ()
sendMail toEmail subj msg = do
  -- Should change this so we don't lookupEnv each time we send an email
  apiKey <- (return . fmap pack) =<< lookupEnv "MANDRILL_API_KEY"
  case apiKey of
    Nothing -> print "You must 'export MANDRILL_API_KEY=\"YOURKEYGOESHERE\"' for sendMail to work"
    Just a -> runMandrill a $ do
      let from = fromJust $ emailAddress "serials@orbit.al"
      let to = fromJust $ emailAddress toEmail
      res <- sendEmail (newHtmlMessage from [to] subj msg)
      case res of
        MandrillSuccess k -> liftIO (print k)
        MandrillFailure f -> liftIO (print f)

welcomeEmail :: User -> Html
welcomeEmail u = do
  H.h3 ("Hello " >> toHtml (U.firstName u) >> ",")
  H.p "Welcome to Serials. Serials aims to be a podcast-like experience for reading serial publications on the web."
  H.p "Many excellent books and webcomics are being published openly on the web today. Most of those are published one chapter at a time. It's awesome that authors are self-publishing, but this format can make it hard to keep track of your progress."
  H.p "Serials lets you subscribe to your favorite works, keep track of your progress, and receive notifications when new content is published"
  H.hr
  H.p $ do
    "Have a suggestion? Want to be notified when we launch? Email us at "
    H.a H.! A.href "mailto:serials@orbit.al" $ "serials@orbit.al"
  H.hr

inviteEmail :: Invite -> Text -> Html
inviteEmail i l = do
  H.h3 "You have been invited to join Serials,"
  H.p $ do
    "Just need to click this link and finishing creating your account: "
    H.a H.! A.href (textValue $ l <> "/#/signup/" <> I.code i) $ "create account"

