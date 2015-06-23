{-# LANGUAGE OverloadedStrings #-}

module Serials.Model.Lib.Crud where

import Database.RethinkDB.NoClash
import Database.RethinkDB.Datum (resultToMaybe)
import Data.Text (Text, unpack)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Safe (headMay)
import Data.Monoid ((<>))
import Data.Pool
import Control.Applicative

import Control.Monad.Trans.Reader
import Control.Monad.Trans (liftIO)

-------------------------------------------------

datumToValue :: (FromDatum a) => Maybe Datum -> Maybe a
datumToValue a = case a of
    Just r -> resultToMaybe $ fromDatum r
    Nothing -> Nothing

generatedKey :: WriteResponse -> Text
generatedKey = head . fromMaybe [""] . writeResponseGeneratedKeys

writeChangeHead :: WriteResponse -> Maybe Change
writeChangeHead rs = (writeResponseChanges rs) >>= headMay

writeChange :: (FromDatum a) => (Change -> Datum) -> WriteResponse -> Maybe a
writeChange v rs = case writeChangeHead rs of
    Just r -> case datumToValue . Just $ v r of
        Just r' -> Just r'
        Nothing -> Nothing
    Nothing -> Nothing

writeChangeNew :: (FromDatum a) => WriteResponse -> Maybe a
writeChangeNew = writeChange newVal

writeChangeOld :: (FromDatum a) => WriteResponse -> Maybe a
writeChangeOld = writeChange oldVal

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



------------------------------------------------------------------

docsList :: FromDatum a => Table -> Pool RethinkDBHandle -> IO [a]
docsList table h = runPool h $ table # orderBy [asc "id"]

docsFind :: FromDatum a => Table -> Pool RethinkDBHandle -> Text -> IO (Maybe a)
docsFind table h id = runPool h $ table # get (expr id)

docsInsert :: ToDatum a => Table -> Pool RethinkDBHandle -> a -> IO Text
docsInsert table h s = do
    r <- runPool h $ table # create s
    return $ generatedKey r

docsSave :: ToDatum a => Table -> Pool RethinkDBHandle -> Text -> a -> IO ()
docsSave table h id s = runPool h $ table # get (expr id) # replace (const (toDatum s))

docsRemove :: Table -> Pool RethinkDBHandle -> Text -> IO ()
docsRemove table h id = runPool h $ table # get (expr id) # delete
