module RedishImpl where

import Data.Map

data Container a = 
    Raw a
  | List [a]
  deriving Show


type DB = Map String (Container String)


