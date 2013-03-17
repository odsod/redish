module Redish where

import Prelude hiding (lookup)
import Data.Map hiding (size, foldr)
import qualified Data.Map as Map
import Data.Monoid

class Sized a where
  size :: a -> Int

instance Sized [a] where 
  size = length

instance Sized (Map k v) where 
  size = Map.size

data Ret v = Val v
           | Vals [v]
           | Len Int
           | OK
           | Nil
  deriving (Eq, Show)

data Command k v =
    -- Keys
    Del [k]
    -- Strings
  | Get k
  | Set k v
  | Append k v
  deriving (Eq, Show)

data Container v =
    Raw v
  | List [v]
  deriving (Eq, Show)
  
newtype DB k v = DB { unDB :: Map k (Container v) }
  deriving (Eq, Show)

emptyDB :: DB k v
emptyDB = DB empty

runCommand :: (Ord k, Monoid v, Sized v) => 
  DB k v -> Command k v -> (Ret (Container v), DB k v)
runCommand db@(DB mdb) cmd = case cmd of
  (Get k) ->
    case lookup k mdb of
      Just v -> (Val v, db)
      Nothing -> (Nil, db)
  (Set k v) -> (OK, DB $ insert k (Raw v) mdb)
  (Del ks) -> let mdb' = (foldr delete mdb ks) 
              in (Len (size mdb - size mdb'), DB mdb')
  (Append k va) -> 
    case lookup k mdb of
      Just (Raw v) -> let v' = (v `mappend` va) 
                      in (Len $ size v', DB $ insert k (Raw v') mdb)
      Just _ -> (Len $ size va, DB $ insert k (Raw va) mdb)
      Nothing -> undefined
