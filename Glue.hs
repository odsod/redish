module Command where

import Control.Concurrent.STM
import Redish

{- Socket -> String -> Command -}

processCommand :: Handle -> (TVar DB) -> IO ()
processCommand handle db = undefined
    {-line <- hGetLine handle-}
    {-let cmd = words line-}
    {-case cmd of-}
        {-"*2":_ -> do-}
            {-_   <- hGetLine handle -- argSize-}
            {-_   <- hGetLine handle -- arg-}
            {-_   <- hGetLine handle -- keySize-}
            {-key <- hGetLine handle-}
            {-getCommand handle key db-}
        {-"*3":_ -> do-}
            {-_     <- hGetLine handle -- argSize-}
            {-_     <- hGetLine handle -- arg-}
            {-_     <- hGetLine handle -- keySize-}
            {-key   <- hGetLine handle-}
            {-_     <- hGetLine handle -- valueSize-}
            {-value <- hGetLine handle-}
            {-setCommand  handle key value db-}
        {-_  -> do hPutStrLn handle "Unknown command"-}
    {-processCommand handle db-}

handleCmd :: String -> (TVar DB) -> STM String
handleCmd cmd tdb = do
  db <- readTVar tdb
  let (res, db') = runCommand db $ readCommand cmd
  writeTVar tdb db'
  return res
  
readCommand :: String -> Command
readCommand = undefined

{-parseCmd :: String -> Either String Command-}
{-parseCmd []     = Left "No command"-}
{-parseCmd w = case w of-}
  {-"get" -> if length ws > 0-}
                  {-then return $ Get ws-}
                  {-else Left "Too few arguments to get"-}
  {-"set"      -> if length ws > 1-}
                  {-then return $ Set ws-}
                  {-else Left "Too few arguments to set"-}
  {-_          -> Left $ "Unknown command: " ++ show (w:ws)-}

