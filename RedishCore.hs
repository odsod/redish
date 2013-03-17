module RedishCore where

import Prelude hiding (lookup)
import Data.Map hiding (size, foldr)
import qualified Data.Map as Map
import Data.Monoid

{-| Typeclass for things that have a (discrete) size |-}
class Sized a where
  {-| Gives the size of the input |-}
  size :: a -> Int

instance Sized [a] where 
  size = length

instance Sized (Map k v) where 
  size = Map.size

{-| A data type representing the different kind of replies
    in the Redis specification.
|-}
data Reply v =
             StatRep String
           | ErrRep String
           | IntRep Int
           | BulkRep v
           | NBulkRep
           | MBulkRep [v]
  deriving (Eq)

{-| A data type representing the different commands
    supported by Redish.
|-}
data Command k v =
    -- Keys
    Exists k
  | Del [k]
    -- Strings
  | Get k
  | Set k v
  | Append k v
  deriving (Eq, Show)

{-| A Container data type for storing different data structures of 
    containing types v.
|-}
data Container v =
    Raw v
  | List [v]
  deriving (Eq, Show)

{-| A DB type mapping keys of type k to Containers of values of
    type v
|-}
newtype DB k v = DB { unDB :: Map k (Container v) }
  deriving (Eq, Show)

{-| Gives an empty DB |-}
emptyDB :: DB k v
emptyDB = DB empty

{-| Runs a given command on a given DB retyrning a tuple
    containing a Reply and the updated DB.
|-}
runCommand :: (Ord k, Monoid v, Sized v) => 
  DB k v -> Command k v -> (Reply (Container v), DB k v)
runCommand db@(DB mdb) cmd = case cmd of
  (Get k) -> case lookup k mdb of
    Just v -> (BulkRep v, db)
    Nothing -> (NBulkRep, db)
  (Set k v) -> (StatRep "OK", DB $ insert k (Raw v) mdb)
  (Exists k) -> (IntRep $ maybe 0 (const 1) (lookup k mdb), db)
  (Del ks) -> let mdb' = (foldr delete mdb ks) 
              in (IntRep (size mdb - size mdb'), DB mdb')
  (Append k va) -> case lookup k mdb of
    Just (Raw v) -> let v' = (v `mappend` va) 
                    in (IntRep $ size v', DB $ insert k (Raw v') mdb)
    Just _ -> (IntRep $ size va, DB $ insert k (Raw va) mdb)
    Nothing -> (IntRep $ size va, DB $ insert k (Raw va) mdb)
