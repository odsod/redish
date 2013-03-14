module RedishNetwork where

import System.IO
import Control.Concurrent (forkIO)
import Control.Concurrent.STM

import RedishCmd

defaultPort :: Int
defaultPort = 7777


handleConns :: Socket -> (TVar DB) -> IO ()
handleConns s db = do
  (handle, host, port) <- accept s -- accept connection
  hSetBuffering handle NoBuffering -- Buffering ??
  cmd <- parseCmd $ hGetLine handle
  case cmd of
    (Left msg)       -> hPutStrLn $ "Invalid command: " ++ msg
                        handleConns s db
    (Right Shutdown) -> hPutStrLn "Shutting down server"
    (Right cmd')     -> handleConn cmd' db
                        handleConns s db

handleConn :: Command -> (TVar DB) -> IO ()
handleConn Shutdown db = return ()
handleConn cmd db      = 
  forkIO $ atomically $ handleCmd cmd db
