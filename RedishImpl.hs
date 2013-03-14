module RedishImpl where

import Prelude hiding (lookup)
import Data.Map

type Key = String
type Value = String

nil :: Value
nil = "(nil)"

ok :: Value
ok = "OK"

data Command =
    -- Strings
    Get Key
  | Set Key Value
  | Append Key Value

runCommand :: DB -> Command -> (String, DB)
runCommand db cmd = case cmd of
  (Get k) -> case lookup k db of
    Just v -> (show v, db)
    Nothing -> (nil, db)
  (Set k v) -> (ok, insert k (Raw v) db)
  (Append k va) -> case lookup k db of
    Just (Raw v) -> let v' = (v ++ va) in 
                    (show $ length va, insert k (Raw v') db)
    Just _ -> (show $ length va, insert k (Raw va) db)
    Nothing -> undefined
  
type DB = Map Key (Container Value)

data Container a =
    Raw a
  {-| List [a]-}
  deriving Show
