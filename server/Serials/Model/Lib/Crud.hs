{-# LANGUAGE OverloadedStrings #-}

module Serials.Model.Lib.Crud where

import Database.RethinkDB.NoClash
import Data.Text (Text, unpack)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Pool
import Control.Applicative

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


-------------------------------------------------------

runPool :: (Expr query, Result r) => Pool RethinkDBHandle -> query -> IO r
runPool p q = withResource p $ \h -> run h q

connectDb :: (String,Integer) -> IO RethinkDBHandle
connectDb (host,port) = use serialsDb <$> connect host port Nothing

disconnectDb :: RethinkDBHandle -> IO ()
disconnectDb h = close h

connectDbPool :: (String, Integer) -> IO (Pool RethinkDBHandle)
connectDbPool hp = createPool (connectDb hp) disconnectDb 1 10 5


-- DB -----------------------------------------------------------

createDb :: Pool RethinkDBHandle -> IO ()
createDb p = initDb $ runPool p $ dbCreate $ unpack serialsDbName

serialsDb = db serialsDbName
serialsDbName = "serials"


