module Server (
    listenOnPort
  , defaultPort
  ) where

import Control.Concurrent.STM
import Control.Concurrent
import System.Environment
import System.IO
import Network

import ConnectionHandler
import Redish

defaultPort :: PortID
defaultPort = PortNumber 6379

listenOnPort :: PortID -> IO ()
listenOnPort port = withSocketsDo $ do
    tdb <- newDB
    sock <- listenOn port
    putStrLn $ "Listening on localhost:" ++ (show port)
    handleSocket sock tdb

handleSocket :: Socket -> (TVar RedishDB) -> IO ()
handleSocket sock tdb = do
    (handle, _, _) <- accept sock
    hSetBuffering handle NoBuffering
    forkIO (processCommand handle tdb)
    handleSocket sock tdb
