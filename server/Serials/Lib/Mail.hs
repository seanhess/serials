{-# LANGUAGE OverloadedStrings #-}

module Serials.Lib.Mail where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Maybe (fromJust)
import Network.API.Mandrill
import Text.Email.Validate (emailAddress)
import Text.Blaze.Html (Html, toHtml, toMarkup)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

sendMail :: ByteString -> Text -> Html -> IO ()
sendMail toEmail subj msg = runMandrill "MANDRILLTOKEN" $ do
  let from = fromJust $ emailAddress "no-reply@serials.orbit.al"
  let to = fromJust $ emailAddress toEmail
  res <- sendEmail (newHtmlMessage from [to] subj msg)
  case res of
    MandrillSuccess k -> liftIO (print k)
    MandrillFailure f -> liftIO (print f)

-- consider just passing user in here so we can use what we want.
-- just need to move things around so we don't get circular problems.
welcomeEmail :: Text -> Html
welcomeEmail name = do
  H.h3 ("Hello " >> toHtml name >> ",")
  H.p "Welcome to Serials. Serials aims to be a podcast-like experience for reading serial publications on the web."
  H.p "Many excellent books and webcomics are being published openly on the web today. Most of those are published one chapter at a time. It's awesome that authors are self-publishing, but this format can make it hard to keep track of your progress."
  H.p "Serials lets you subscribe to your favorite works, keep track of your progress, and receive notifications when new content is published"
  H.hr
  H.p $ do
    "Have a suggestion? Want to be notified when we launch? Email us at "
    H.a H.! A.href "mailto:serials@orbit.al" $ "serials@orbit.al"
  H.hr
