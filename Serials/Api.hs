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
import Data.Text (Text, toUpper)
import Data.Maybe (fromJust)

import GHC.Generics

import Network.Wai
import Network.Wai.Handler.Warp (run, Port)
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.AddHeaders

import Serials.Model.Source

import Servant
import Servant.JQuery

import Database.RethinkDB.NoClash (RethinkDBHandle, use, db, connect)


-- * Example

-- | A greet message data type
newtype Greet = Greet { _msg :: Text }
  deriving (Generic, Show)

instance FromJSON Greet
instance ToJSON Greet


-- API specification
type Api =

       "hello" :> Capture "name" Text :> QueryParam "capital" Bool :> Get Greet

  :<|> "greet" :> ReqBody Greet :> Post Greet
  :<|> "greet" :> Capture "greetid" Text :> Delete


  :<|> "sources" :> Get [Source]
  :<|> "sources" :> ReqBody Source :> Post Text

  :<|> "sources" :> Capture "id" Text :> Get (Source)
  :<|> "sources" :> Capture "id" Text :> ReqBody Source :> Put ()
  :<|> "sources" :> Capture "id" Text :> Delete


api :: Proxy Api
api = Proxy

-- Server-side handlers.
--
-- There's one handler per endpoint, which, just like in the type
-- that represents the API, are glued together using :<|>.
--
-- Each handler runs in the 'EitherT (Int, String) IO' monad.
server :: RethinkDBHandle -> Server Api
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
app :: RethinkDBHandle -> Application
app conn = serve api (server conn)

runTestServer :: Port -> RethinkDBHandle -> IO ()
runTestServer port h = run port $ heads $ cors' $ app h
  where
    heads = addHeaders [("X-Hacker", "Hi there! Consider contributing at http://github.com/seanhess/serials")]

-- Run ---------------------------------------------------------

runApi :: Int -> IO ()
runApi port = do
    --writeJS "test.js" [helloH, postGreet, delGreet]
    print $ toJS [getSource, getSources, postSources]
    --putStrLn $ "Running on " <> show port
    --h <- connectDb
    --runTestServer port h
    --return ()

-- DB -----------------------------------------------------------

connectDb :: IO RethinkDBHandle
connectDb = use serialsDb <$> connect "localhost" 28015 Nothing

serialsDb = db serialsDbName
serialsDbName = "serials"

-- Cors ---------------------------------------------------------

cors' :: Middleware
cors' = cors (const $ Just corsResourcePolicy)

corsResourcePolicy :: CorsResourcePolicy
corsResourcePolicy = CorsResourcePolicy
    { corsOrigins = Nothing
    , corsMethods = ["GET", "HEAD", "OPTIONS", "POST", "PUT", "DELETE"]
    , corsRequestHeaders = simpleResponseHeaders
    , corsExposedHeaders = Nothing
    , corsMaxAge = Nothing
    , corsVaryOrigin = False
    , corsRequireOrigin = False
    , corsIgnoreFailures = False
    }

-- JS API --------------------------------------------------------

writeJS :: FilePath -> [AjaxReq] -> IO ()
writeJS fp functions = writeFile fp $ toJS functions

toJS :: [AjaxReq] -> String
toJS = concatMap generateJS

helloH :<|> postGreet :<|> delGreet :<|> getSources :<|> postSources :<|> getSource :<|> _ = jquery api

