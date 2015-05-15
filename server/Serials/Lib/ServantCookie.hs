{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module Serials.Lib.ServantCookie where

import Data.Text (Text)
import           Data.String                 (fromString)
import Data.ByteString (ByteString)
import Data.Typeable   (Typeable)
import GHC.TypeLits    (Symbol, KnownSymbol, symbolVal)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.Wai

import Servant

import Web.Cookie


-- | Extract the given header's value as a value of type @a@.
--
-- Example:
--
-- >>>            -- GET /test
-- >>> type MyApi = "test" :> Cookie "token" Text :> Get '[JSON] Text

data Cookie (sym :: Symbol) a = Cookie a
                              | MissingCookie
                              | UndecodableCookie ByteString
                              deriving (Typeable, Eq, Show, Functor)

-- | If you use 'Cookie' in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of the type specified by 'Cookie.
-- This lets servant worry about extracting it from the request and turning
-- it into a value of the type you specify.
--
-- All it asks is for a 'FromText' instance.
--
-- Example:
--
-- >            -- GET /test
-- > type MyApi = "test" :> Cookie "token" Text :> Get '[JSON] Text
-- >
-- > server :: Server MyApi
-- > server = test
-- >   where test :: Maybe Text -> EitherT ServantErr IO Text
-- >         test token = return token
instance (KnownSymbol sym, FromText a, HasServer sublayout)
      => HasServer (Cookie sym a :> sublayout) where

  type ServerT (Cookie sym a :> sublayout) m =
    Maybe a -> ServerT sublayout m

  route Proxy subserver request respond = do
    let mheader = lookup "cookie" (requestHeaders request) :: Maybe ByteString
        mc = fromText =<< lookup str =<< fmap parseCookiesText mheader
    route (Proxy :: Proxy sublayout) (subserver mc) request respond
      where str = fromString $ symbolVal (Proxy :: Proxy sym)

