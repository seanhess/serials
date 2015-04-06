
module Main where

import Control.Concurrent
import System.IO
import Control.Monad
import Crawl
import Scrape

main = testCrawl

--------------------------------------------

forkChars = do
  hSetBuffering stdout NoBuffering
  forkIO (replicateM_ 1000 (putChar 'A'))
  replicateM_ 1000 (putChar 'B')

--------------------------------------------

mvars = do
  m <- newEmptyMVar
  forkIO $ putMVar m 'x'
  r <- takeMVar m
  print r

---------------------------------------------

data Logger = Logger (MVar LogCommand)
data LogCommand = Message String | Stop (MVar ())

initLogger :: IO Logger
initLogger = do
  m <- newEmptyMVar
  let l = Logger m
  forkIO (logger l)
  return l

logger :: Logger -> IO ()
logger (Logger m) = loop
  where
    loop = do
      cmd <- takeMVar m
      case cmd of
        Message msg -> do
          putStrLn msg
          loop
        Stop s -> do
          putStrLn "logger: stop"
          putMVar s ()

logMessage :: Logger -> String -> IO ()
logMessage (Logger m) s = putMVar m (Message s)

logStop :: Logger -> IO ()
logStop (Logger m) = do
  s <- newEmptyMVar
  putMVar m (Stop s)
  takeMVar s

testLogger = do
  l <- initLogger
  logMessage l "hello"
  logMessage l "bye"
  logStop l


---------------------------------------------------
-- New Strategy
-- functions that return a list of potential lists
-- just IO functions of any type
-- different things work for different sites
--------------------------------------------------
