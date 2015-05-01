{-# LANGUAGE OverloadedStrings #-}

module Serials.Lib.Helpers where

import Data.ByteString.Char8 (pack)
import Data.Char (isAscii, isAlphaNum)
import System.Random (randoms, newStdGen)
import Crypto.BCrypt
import Control.Monad (liftM)

randomString :: IO String
randomString = do
  stdgen <- newStdGen
  return . take 64 $ filter (\v -> isAscii v && isAlphaNum v) $ randoms stdgen

customHashPolicy :: HashingPolicy
customHashPolicy = HashingPolicy 10 (pack "$2b$")

