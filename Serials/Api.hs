{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Serials.Api where

import Control.Applicative 
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either

import Data.Aeson
import Data.Monoid
import Data.Proxy
import Data.Text
import Data.Maybe (fromJust)

import GHC.Generics

import Network.Wai
import Network.Wai.Handler.Warp

import Serials.Model.Source

import Servant

import Database.RethinkDB.NoClash (RethinkDBHandle, use, db, connect)


-- * Example

-- | A greet message data type
newtype Greet = Greet { _msg :: Text }
  deriving (Generic, Show)

instance FromJSON Greet
instance ToJSON Greet


-- API specification
type TestApi =
       -- GET /hello/:name?capital={true, false}  returns a Greet as JSON
       "hello" :> Capture "name" Text :> QueryParam "capital" Bool :> Get Greet

       -- POST /greet with a Greet as JSON in the request body,
       --             returns a Greet as JSON
  :<|> "greet" :> ReqBody Greet :> Post Greet

       -- DELETE /greet/:greetid
  :<|> "greet" :> Capture "greetid" Text :> Delete


  :<|> "sources" :> Get [Source]

  :<|> "sources" :> ReqBody Source :> Post Text

  :<|> "sources" :> Capture "id" Text :> Get (Source)

  :<|> "sources" :> Capture "id" Text :> ReqBody Source :> Put ()

  :<|> "sources" :> Capture "id" Text :> Delete


testApi :: Proxy TestApi
testApi = Proxy

-- Server-side handlers.
--
-- There's one handler per endpoint, which, just like in the type
-- that represents the API, are glued together using :<|>.
--
-- Each handler runs in the 'EitherT (Int, String) IO' monad.
server :: RethinkDBHandle -> Server TestApi
server h = 
    helloH :<|> postGreetH :<|> deleteGreetH :<|> 
    sourcesGetAll :<|> sourcesPost :<|> sourcesGet :<|> sourcesPut :<|> sourcesDelete

  where 

  helloH name Nothing = helloH name (Just False)
  helloH name (Just False) = return . Greet $ "Hello, " <> name
  helloH name (Just True) = return . Greet . toUpper $ "Hello, " <> name

  postGreetH greet = return greet

  deleteGreetH _ = return ()

  sourcesGetAll :: EitherT (Int, String) IO [Source]
  sourcesGetAll = liftIO $ sourcesList h

  sourcesPost s = liftIO $ sourcesCreate h s

  sourcesGet :: Text -> EitherT (Int, String) IO (Source)
  sourcesGet id = EitherT $ notFound <$> sourcesFind h id

  sourcesPut id s  = liftIO $ sourcesSave h id s

  sourcesDelete id = liftIO $ sourcesRemove h id


notFound :: Maybe a -> Either (Int, String) a
notFound Nothing  = Left (404, "Not Found")
notFound (Just a) = Right a

-- Turn the server into a WAI app. 'serve' is provided by servant,
-- more precisely by the Servant.Server module.
test :: RethinkDBHandle -> Application
test conn = serve testApi (server conn)

serialsDb = db serialsDbName
serialsDbName = "serials"

runTestServer :: Port -> RethinkDBHandle -> IO ()
runTestServer port h = run port (test h)

runApi :: Int -> IO ()
runApi port = do
    putStrLn $ "Running on " <> show port
    h <- use serialsDb <$> connect "localhost" 28015 Nothing
    runTestServer port h
    return ()
