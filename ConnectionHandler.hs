module ConnectionHandler where

import Control.Concurrent.STM
import Control.Applicative
import System.IO

import Redish

newDB :: IO (TVar RedishDB)
newDB = newTVarIO emptyDB

processCommand :: Handle -> TVar RedishDB -> IO ()
processCommand handle tdb = do
    numArgs <- ((tail . head . words) <$> hGetLine handle)
    let numLines = ((read numArgs) :: Int) * 2
    input <- sequence (replicate numLines (hGetLine handle))
    reply <- atomically $ handleCommand input tdb
    hPutStr handle reply
    processCommand handle tdb

handleCommand :: [String] -> TVar RedishDB -> STM String
handleCommand input tdb = do
  db <- readTVar tdb
  let (rep, db') = interpretCommand db input
  writeTVar tdb db'
  return (show rep)
