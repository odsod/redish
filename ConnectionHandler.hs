module Network where

import System.IO
import Control.Concurrent (forkIO)
import Control.Concurrent.STM

import Command

handleConns :: Socket -> (TVar DB) -> IO ()
handleConns s db = do
  (handle, host, port) <- accept s -- accept connection
  hSetBuffering handle NoBuffering -- Buffering ??
  forkIO $ handleConn handle db
  handleConns s db

handleConn :: Handle -> (TVar DB) -> IO ()
handleConn h db = do
  cmd <- hGetLine handle
  res <- atomically $ handleCmd cmd db
  hPutStrLn h res
  handleConn h db -- How, what, why!?!
