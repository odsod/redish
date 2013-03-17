module Command where

import Control.Concurrent.STM
import Control.Applicative
import System.IO
import Redish
import StringInterface

processCommand :: Handle -> (TVar RedishDB) -> IO ()
processCommand handle tdb = do
    numArgs <- ((tail . head . words) <$> hGetLine handle)
    let numLines = ((read numArgs) :: Int) * 2
    input <- sequence (replicate numLines (hGetLine handle))
    reply <- atomically $ handleCommand (parseCommand input) tdb
    hPutStr handle reply
    processCommand handle tdb

handleCommand :: RedishCMD -> TVar RedishDB -> STM String
handleCommand cmd tdb = do
  db <- readTVar tdb
  let (rep, db') = runCommand db cmd
  writeTVar tdb db'
  return (show rep)
