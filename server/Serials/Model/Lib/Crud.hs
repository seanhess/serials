{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}


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

import Control.Monad.Reader
import Control.Monad.Trans (liftIO)

import Serials.Types

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

class (MonadIO m) => RethinkIO m where
  connPool :: m (Pool RethinkDBHandle)

instance RethinkIO App where
  connPool = asks conn

runDb :: (Expr query, Result r, RethinkIO m) => query -> m r
runDb q = do
    pool <- connPool
    liftIO $ withResource pool $ \h -> run h q

-------------------------------------------------------

initDb :: App (Either RethinkDBError Datum) -> App ()
initDb action = do
    r <- action
    liftIO $ putStrLn $ "[INIT] " <> case r of
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

createDb :: App ()
createDb = initDb $ runDb $ dbCreate $ serialsDbName

serialsDb :: Database
serialsDb = db serialsDbName

serialsDbName :: Text
serialsDbName = "serials"



------------------------------------------------------------------

docsList :: FromDatum a => Table -> App [a]
docsList table = runDb $ table # orderBy [asc "id"]

docsFind :: FromDatum a => Table -> Text -> App (Maybe a)
docsFind table id = runDb $ table # get (expr id)

docsInsert :: ToDatum a => Table -> a -> App Text
docsInsert table s = do
    r <- runDb $ table # create s
    return $ generatedKey r

docsSave :: ToDatum a => Table -> Text -> a -> App ()
docsSave table id s = runDb $ table # get (expr id) # replace (const (toDatum s))

docsRemove :: Table -> Text -> App ()
docsRemove table id = runDb $ table # get (expr id) # delete

--------------------------------------------------------------------

