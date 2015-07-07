{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Serials.Route.Test where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Monoid
import Data.Aeson
import Servant.Server
import Servant
import Database.RethinkDB.NoClash
import System.Environment

-- I should have some kind of error messages
newtype App a = App {
  runApp :: ReaderT Int IO a
} deriving (Monad, Functor, Applicative, MonadReader Int)

-- different kinds of things
-- unauthorized

-- could I handle it with the a though? Like before?

-- 404 Missing (Maybe)
-- 400 Invalid "you gave me bad input"   ()
-- 500 Error "some kind of server error" (Either Text a) (or could be an exception?)

-- but these are servant specific
-- Unauthorized
-- Forbidden

-- my thing should allow me to return different kinds of errors that I can handle in different ways
-- let's use some kind of error handling whatzit?

type TestAPI =
         "a" :> Get '[JSON] String
    :<|> "b" :> Get '[JSON] String

--test2 :: EitherT ServantErr IO String
--test2 = return "asdf"

--testServer :: Int -> Server TestAPI
--testServer code = test :<|> test2
  --where
    --test :: EitherT ServantErr IO String
    --test = liftIO $ runReaderT (runApp giveMeAMessage) code

giveMeAMessage :: App String
giveMeAMessage = App $ do
    code <- ask
    name <- liftIO $ getProgName
    return $ show code <> name

---------------------------------------------------------------
-- how do I do this?

testServerT ::ServerT TestAPI App
testServerT = test :<|> test
  where

    test :: App String
    test = giveMeAMessage

-- see this is awesome because I can easily map error codes here
-- especially if they are different types?
runReaderApp :: Int -> App a -> EitherT ServantErr IO a
runReaderApp code action = do
    res <- liftIO $ runReaderT (runApp action) code
    return res

testServer' :: Int -> Server TestAPI
testServer' code = enter (Nat $ (runReaderApp code)) testServerT

--type ReaderAPI = "a" :> Get '[JSON] Int
            -- :<|> "b" :> Get '[JSON] String

--readerServerT :: ServerT ReaderAPI (Reader String)
--readerServerT = return 1797 :<|> asdf
  --where

  --asdf :: EitherT ServantErr (Reader String String)
  --asdf = ask

--readerServer :: Server ReaderAPI
--readerServer = enter (Nat $ return . (`runReader` "hi")) readerServerT


