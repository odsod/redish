module Main where

import System.Environment
import System.IO
import Control.Concurrent.STM

import ConnectionHandler

defaultPort :: PortNumber
defaultPort = PortNumber (fromIntegral 7777)

main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    tdb <- newTVarIO emptyDB
    sock <- listenOn defaultPort
    putStrLn $ "Listening on localhost:" ++ (show defaultPort)
    handleSocket sock tdb

handleSocket :: Socket -> (TVar DB) -> IO ()
handleSocket sock tdb = do
    (handle, _, _) <- accept sock
    hSetBuffering handle NoBuffering
    forkIO (processCommand handle tdb)
    handleSocket sock tdb
