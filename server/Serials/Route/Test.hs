{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Serials.Route.Test where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Monoid
import Data.Aeson
import Servant.Server
import Servant
import Database.RethinkDB.NoClash
import System.Environment


type TestAPI =
         "a" :> Get '[JSON] String
    :<|> "b" :> Get '[JSON] String

test2 :: EitherT ServantErr IO String
test2 = return "asdf"

testServer :: Int -> Server TestAPI
testServer code = test :<|> test2
  where
    test :: EitherT ServantErr IO String
    test = liftIO $ runReaderT (giveMeAMessage) code

giveMeAMessage :: MonadIO m => ReaderT Int m String
giveMeAMessage = do
    code <- ask
    name <- liftIO $ getProgName
    return $ show code <> name

---------------------------------------------------------------
-- how do I do this?

testServerT ::ServerT TestAPI (ReaderT Int (EitherT ServantErr IO))
testServerT = test :<|> test
  where

    test :: ReaderT Int (EitherT ServantErr IO) String
    test = giveMeAMessage

testServer' :: Int -> Server TestAPI
testServer' code = enter (Nat $ (`runReaderT` code)) testServerT

--type ReaderAPI = "a" :> Get '[JSON] Int
            -- :<|> "b" :> Get '[JSON] String

--readerServerT :: ServerT ReaderAPI (Reader String)
--readerServerT = return 1797 :<|> asdf
  --where

  --asdf :: EitherT ServantErr (Reader String String)
  --asdf = ask

--readerServer :: Server ReaderAPI
--readerServer = enter (Nat $ return . (`runReader` "hi")) readerServerT


