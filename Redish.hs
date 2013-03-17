module Redish (
    RedishDB
  , interpretCommand
  , emptyDB
  ) where

import RedishCore

type RedishDB = DB String String
type RedishCMD = Command String String

keepOdd :: [a] -> [a]
keepOdd = snd . unzip . filter (odd . fst) . (zip [0..])

parseCommand :: String -> RedishCMD
parseCommand input = let (c:args) = words input in case c of
  "GET" -> Get (args !! 0)
  "SET" -> Set (args !! 0) (args !! 1)
  "DEL" -> Del args
  "APPEND" -> Append (args !! 0) (args !! 1)

interpretCommand :: RedishDB -> String -> (String, RedishDB)
interpretCommand db input = 
  let (r, db') = runCommand db (parseCommand input)
  in (showReply r, db')

showReply :: Reply (Container String) -> String
showReply r = case r of
  StatRep s -> '+':s
  ErrRep s -> '-':s
  IntRep i -> ':':(show i)
  BulkRep c -> showContainer c
  MBulkRep cs -> unlines (map showContainer cs)
  NBulkRep -> "(nil)"

showContainer :: Container String -> String
showContainer c = case c of
  Raw s -> s
  List s -> unwords s
