{-# LANGUAGE OverloadedStrings #-}

module Serials.Model.Crud where

import Database.RethinkDB.NoClash
import Data.Text (Text)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))

import Control.Monad.Trans.Reader
import Control.Monad.Trans (liftIO)

-------------------------------------------------

generatedKey :: WriteResponse -> Text
generatedKey = head . fromMaybe [""] . writeResponseGeneratedKeys

stripId :: Datum -> Datum
stripId (Object o) = Object $ HM.delete "id" o
stripId x = x

create :: ToDatum a => a -> Table -> ReQL
create o = insert (stripId $ toDatum o)

toDatumNoId :: ToDatum a => a -> Datum
toDatumNoId = stripId . toDatum

-------------------------------------------------

runDb :: (Expr a, Result r) => a -> RethinkIO r
runDb e = do
    h <- ask
    liftIO $ run h e

type RethinkIO = ReaderT RethinkDBHandle IO

-------------------------------------------------------

initDb :: IO (Either RethinkDBError Datum) -> IO ()
initDb action = do
    r <- action
    putStrLn $ "[INIT] " <> case r of
      Left err -> errorMessage err
      Right d  -> show d


