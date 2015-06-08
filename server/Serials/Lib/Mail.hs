{-# LANGUAGE OverloadedStrings #-}

module Serials.Lib.Mail (
  sendMail,
  logoPage,
  isValidAddress,
  Email(..)
) where

import Prelude hiding (div)

import System.Environment
import Data.Monoid ((<>))
import Data.Maybe (isJust)
import Data.ByteString (ByteString)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Maybe (fromJust)
import Network.API.Mandrill
import Text.Email.Validate (emailAddress, isValid)
import Text.Blaze (textValue)
import Text.Blaze.Html (Html, toHtml, toMarkup)

import Serials.Model.User (User)
import qualified Serials.Model.User as U
import Serials.Model.Invite (Invite)
import qualified Serials.Model.Invite as I

import Serials.Model.App (readAllEnv, Env(..), Endpoint)

import Text.Blaze.Html5 hiding (style, map)
import Text.Blaze.Html5.Attributes

data Email = Email {
  emailSubject :: Text,
  emailBody :: Html
}

isValidAddress :: Text -> Bool
isValidAddress = isValid . encodeUtf8

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

logoPage :: Endpoint -> Html -> Html
logoPage endpoint body = do
  div ! style "max-width: 420px" $ do
    div ! style "text-align: center" $ do
      img ! src (textValue $ endpoint <> "/img/serials-logo-dark.png") ! style "width: 100px"
    body
