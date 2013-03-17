module StringInterface where

import Redish

type RedishDB = DB String String
type RedishCMD = Command String String

keepOdd :: [a] -> [a]
keepOdd = snd . unzip . filter (odd . fst) . (zip [0..])

parseCommand :: [String] -> RedishCMD
parseCommand input = let (c:args) = keepOdd input in case c of
  "GET" -> Get (args !! 0)
  "SET" -> Set (args !! 0) (args !! 1)
  "DEL" -> Del args
  "APPEND" -> Append (args !! 0) (args !! 1)
