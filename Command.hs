module Command where

import Control.Concurrent.STM
import Redish

handleCmd :: String -> (TVar DB) -> STM String
handleCmd cmd tdb = do
  db <- readTVar tdb
  let (res, db') = runCommand db $ readCommand cmd
  writeTVar tdb db'
  return res
  
readCommand :: String -> Command
readCommand = undefined

parseCmd :: String -> Either String Command
parseCmd []     = Left "No command"
parseCmd (w:ws) = 
  case w of
    "get"      -> if length ws > 0
                    then return $ Get ws
                    else Left "Too few arguments to get"
    "set"      -> if length ws > 1
                    then return $ Set ws
                    else Left "Too few arguments to set"
    _          -> Left $ "Unknown command: " ++ show (w:ws)
