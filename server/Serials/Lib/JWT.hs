{-# LANGUAGE OverloadedStrings #-}

module Serials.Lib.JWT (
  subject,
  toUTCTime,
  defaultClaims,
  signClaims,
  verifyJwt
) where

import Control.Applicative ((<$>))

import Prelude hiding (exp, id)
import Data.Time.Clock
import Data.Monoid
import Data.Time.Clock.POSIX
import Data.Aeson (toJSON, Value)
import System.Posix.Types
import Data.Text (Text, pack)
import Data.Maybe (fromJust)
import Data.Aeson (toJSON)
import Web.JWT
import Data.Map.Lazy (Map, fromList)

oneDay :: NominalDiffTime
oneDay = 24 * 60 * 60

oneYear = 365 * oneDay

addTime :: NominalDiffTime -> UTCTime -> UTCTime
addTime d t = d `addUTCTime` t

toSecs :: UTCTime -> Int
toSecs = round . utcTimeToPOSIXSeconds

toUTCTime :: IntDate -> UTCTime
toUTCTime i = posixSecondsToUTCTime $ secondsSinceEpoch i

utcTimeToEpochTime :: UTCTime -> NominalDiffTime
utcTimeToEpochTime = fromIntegral . toSecs

timeToIntDate :: UTCTime -> Maybe IntDate
timeToIntDate t = intDate $ utcTimeToEpochTime t

timeLater :: NominalDiffTime -> IO (Maybe IntDate)
timeLater dt = do
    time <- getCurrentTime
    return . timeToIntDate $ addTime dt time

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

subject :: JWT a -> Maybe StringOrURI
subject = sub . claimSet

defaultClaims :: Text -> Map Text Value -> IO JWTClaimsSet
defaultClaims id unc = do
  iat <- currentTime
  exp <- timeLater oneYear
  return $ def {
      iss = stringOrURI "Serials",
      iat = iat,
      sub = stringOrURI id,
      exp = exp,
      unregisteredClaims = unc
    }

signClaims :: Secret -> JWTClaimsSet -> JSON
signClaims secret cs = encodeSigned HS256 secret cs

verifyJwt :: Secret -> JSON -> IO (Maybe (JWT VerifiedJWT))
verifyJwt secret j = case verify secret =<< decode j of
  Nothing -> return Nothing
  Just j -> do
    exp <- hasExpired j
    if exp then return Nothing
    else return $ Just j

hasExpired :: JWT a -> IO Bool
hasExpired j = case expiredAt j of
  Nothing -> return False
  Just e  -> do
    let expTime = toUTCTime e
    time <- getCurrentTime
    return $ expTime < time
