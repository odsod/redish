{-| Test suite for the RedishCore module. |-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import System.Exit
import Test.QuickCheck
import Test.QuickCheck.Test
import Control.Monad
import Control.Applicative
import Control.Monad.Writer
import Data.Monoid
import Control.Monad.State
import qualified Data.Map as Map

import RedishCore

instance (Arbitrary a) => Arbitrary (Container a) where
  arbitrary = arbitraryContainer

arbitraryContainer :: (Arbitrary a) => Gen (Container a)
arbitraryContainer = oneof 
  [ liftM Raw arbitrary
  , liftM List arbitrary ]

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (DB k v) where
  arbitrary = liftM (DB . Map.fromList) arbitrary

{-| A State of a DB for testing statuful operations  |-}
type DBTester a = State TestDB a

type TestDB = DB String String
type TestCommand = Command String String
type TestReply = Reply (Container String)

{-| Runs one command on a TestDB |-}
runTC :: TestCommand -> DBTester TestReply
runTC c = do
  db <- get
  let (r, db') = runCommand db c
  put db'
  return r

{-| Run function for DBTester |-}
runDBT :: TestDB -> DBTester a -> a
runDBT = flip evalState

-----------------------------------------------------------------------

-- puts a value into the db with a key, get the value with that key
-- check that the retrieved value matches the inserted value
prop_set_get :: String -> String -> TestDB -> Bool
prop_set_get k v db = runDBT db $ do
  runTC $ Set k v
  liftM2 (==) (runTC $ Get k)
              (return $ BulkRep $ Raw v)

-- does 'get' with the same key twice in a row, checks that
-- both gets give the same result
prop_get_get :: String -> String -> TestDB -> Bool
prop_get_get k v db = runDBT db $
  liftM2 (==) (runTC $ Get k) 
              (runTC $ Get k)

-- checks that 'append' on an entry in the db works.
-- checks both that the append returns the correct
-- value (the length of the entry after the append)
-- and that the entry is the "original" value with
-- the "append value" appended.
prop_set_append :: String -> String -> String -> TestDB -> Bool
prop_set_append k v a db = runDBT db $ do
  runTC $ Set k v
  l <- runTC $ Append k a
  r <- runTC $ Get k
  return $  r == (BulkRep $ Raw $ v ++ a) 
         && l == (IntRep $ length (v ++ a))

-- checks that a key that has been set in the db exists
prop_set_exists :: String -> String -> TestDB -> Bool
prop_set_exists k v db = runDBT db $ do
  runTC $ Set k v
  liftM2 (==) (runTC $ Exists k) 
              (return $ IntRep 1)

-- checks that an append on a nonexisting entry in the 
-- db has the same affect as a set
prop_del_app_eq_set :: String -> String -> TestDB -> Bool
prop_del_app_eq_set k v db = runDBT db $ do
  runTC $ Del [k]
  runTC $ Append k v
  r1 <- runTC $ Get k
  runTC $ Set k v
  r2 <- runTC $ Get k
  return $ r1 == r2 

-- checks that an entry that has been deleted does not exist
prop_set_del_exists :: String -> String -> TestDB -> Bool
prop_set_del_exists k v db = runDBT db $ do
  runTC $ Set k v
  runTC $ Del [k]
  liftM2 (==) (runTC $ Exists k) 
              (return $ IntRep 0)

-- checks that get on a key that does not exist in the db
-- gives a proper null representation as a result
prop_del_get :: String -> TestDB -> Bool
prop_del_get k db = runDBT db $ do
  runTC $ Del [k]
  liftM2 (==) (runTC $ Get k)
              (return NBulkRep)

------------------------------------------------------------------------

{-| A Checker monad for checking several quickCheck props
    and storing the result of each check.
|-}
newtype Checker a = Checker { unChecker :: WriterT [Result] IO a }
  deriving ( Monad, MonadWriter [Result], MonadIO )

{-| Run function for Checker |-}
runChecker :: Checker () -> IO [Result]
runChecker = liftM snd . runWriterT . unChecker

{-| QuickChecks one quickCheck property and stores the result 
    in the Checker monad 
|-}
check :: Testable prop => prop -> Checker ()
check p = liftIO (quickCheckResult p) >>= tell . (:[])

{-| Checks several properties, saving the result of each one 
    inside the Checker monad.
|-}
checks :: Checker ()
checks = do
  check prop_set_get
  check prop_get_get
  check prop_set_append
  check prop_set_del_exists
  check prop_set_exists
  check prop_del_app_eq_set
  check prop_del_get

{-| Main function for the test suite. 
    Checks all our quickCheck properties 
|-}
main :: IO ()
main = runChecker checks >>= \rs ->
  unless (all isSuccess rs) exitFailure
