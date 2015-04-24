{-# LANGUAGE OverloadedStrings #-}

module Serials.Model.Crud where

import Database.RethinkDB
import Database.RethinkDB.NoClash
import Database.RethinkDB.ReQL (Static)
import Data.Text (Text, unpack)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Control.Applicative ((<$>))
import Control.Exception (SomeException(..), try)

import Control.Monad.Trans.Reader
import Control.Monad.Trans (liftIO)

-------------------------------------------------

-- Database
database :: Database
database = Database "serials"

-- Tables
tables :: [Table]
tables = [
    "chapters"
    , "sources"
    ]

-- Indexes
indexes :: [(Text, String, ReQL -> ReQL, [Attribute Static])]
indexes = [
    ("chapters", "sourceId", (!"sourceId"), [])
    ]

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

-- I could just read it here.. it's easy
connectDb :: (String,Integer) -> IO RethinkDBHandle
connectDb (host,port) = use database <$> connect host port Nothing

runDb :: (Expr a, Result r) => a -> RethinkIO r
runDb e = do
    h <- ask
    liftIO $ run h e

type RethinkIO = ReaderT RethinkDBHandle IO

-------------------------------------------------------

try' :: IO a -> IO (Either SomeException a)
try' = try

tryOutput :: IO (Either SomeException Datum) -> IO ()
tryOutput r = r >>= \a -> putStrLn $ "[INIT] " <> case a of
    Left err -> show err
    Right d  -> show d

-- Try and create tables
createTable :: Table -> RethinkDBHandle -> IO ()
createTable t h = tryOutput <$> try' . run' h $ tableCreate t

-- Try and create indexes
createIndex :: (Text, String, ReQL -> ReQL, [Attribute Static]) -> RethinkDBHandle -> IO ()
createIndex (t, f, r, o) h = tryOutput <$> try' $ run' h $ table t # ex indexCreate o f r

initDb :: RethinkDBHandle -> IO ()
initDb h = do
    tryOutput <$> try' . run' h . dbCreate . unpack $ databaseName database
    mapM_ (\t -> createTable t h) tables
    mapM_ (\i -> createIndex i h) indexes

