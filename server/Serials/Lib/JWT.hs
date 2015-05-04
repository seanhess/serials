{-# LANGUAGE OverloadedStrings #-}

module Serials.Lib.JWT (
  subject
  , signedJwtWebToken
  , verifyJwt
  ) where

import Prelude hiding (exp, id)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import System.Posix.Types
import Data.Text (Text, pack)
import Data.Maybe (fromJust)
import Data.Aeson (toJSON)
import Web.JWT
import qualified Data.Map as Map

import Serials.Model.User

oneDay :: NominalDiffTime
oneDay = 24 * 60 * 60

addTime :: NominalDiffTime -> UTCTime -> UTCTime
addTime d t = d `addUTCTime` t

toSecs :: UTCTime -> Int
toSecs = round . utcTimeToPOSIXSeconds

utcTimeToEpochTime :: UTCTime -> NominalDiffTime
utcTimeToEpochTime = fromIntegral . toSecs

timeToIntDate :: UTCTime -> Maybe IntDate
timeToIntDate t = intDate $ utcTimeToEpochTime t

oneDayLater :: IO (Maybe IntDate)
oneDayLater = do
    time <- getCurrentTime
    return . timeToIntDate $ addTime oneDay time

currentTime :: IO (Maybe IntDate)
currentTime = do
    time <- getCurrentTime
    return $ timeToIntDate time

claimSet :: JWT a -> JWTClaimsSet
claimSet = claims

expiredAt :: JWT a -> Maybe IntDate
expiredAt = exp . claimSet

intDateToNominalDT :: Maybe IntDate -> NominalDiffTime
intDateToNominalDT = secondsSinceEpoch . fromJust

subject :: Maybe (JWT a) -> Maybe StringOrURI
subject (Just j) = sub $ claimSet j
subject Nothing = Nothing

signedJwtWebToken :: User -> IO JSON
signedJwtWebToken user = do
    o <- oneDayLater
    signedJwtToken o user

jwtSecret :: Secret
jwtSecret = secret "6aefad90e7a41c1d9267feccc0ee763ebd8ef9c3496a2d84b5c36e6ff4b7534dce5dbb705254d6a0253f3ccf36300bb0f1b6e1c4ab805c3c85884e3df3dbd0c7"

signedJwtToken :: Maybe IntDate -> User -> IO JSON
signedJwtToken exp user = do
    let userId = id user
    iat <- currentTime
    let cs = def {
        iss = stringOrURI "Serials"
        , iat = iat
        , sub = stringOrURI userId
        , exp = exp
        }
    return $ encodeSigned HS256 jwtSecret cs

verifyJwt :: JSON -> IO (Maybe (JWT VerifiedJWT))
verifyJwt j = case verify jwtSecret =<< decode j of
  Nothing -> return Nothing
  Just j -> fmap f $ expired j
    where f r = case r of
            Just True -> Just j
            _ -> Nothing

expired :: JWT a -> IO (Maybe Bool)
expired j = do
    t <- currentTime
    let e = expiredAt j
    return . Just $ intDateToNominalDT e > intDateToNominalDT t

