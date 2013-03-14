module Command where

import Control.Concurrent.STM
import Redish

handleCmd :: Command -> (TVar DB) -> STM ()
handleCmd cmd db = do
  db' <- readTVar db
  case cmd of
    (Get (id:_))     -> 
    (Set (id:val:_)) -> 

parseCmd :: String -> Either String Command
parseCmd []     = Left "No command"
parseCmd (w:ws) = 
  case w of
    "shutdown" -> Shutdown
    "get"      -> if length ws > 0
                    then return $ Get ws
                    else Left "Too few arguments to get"
    "set"      -> if length ws > 1
                    then return $ Set ws
                    else Left "Too few arguments to set"
    _          -> Left $ "Unknown command: " ++ show (w:ws)
