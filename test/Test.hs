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

type DBTester a = State TestDB a
type TestDB = DB String String
type TestCommand = Command String String
type TestReply = Reply (Container String)

runTC :: TestCommand -> DBTester TestReply
runTC c = do
  db <- get
  let (r, db') = runCommand db c
  put db'
  return r

runDBT :: TestDB -> DBTester a -> a
runDBT = flip evalState

arbitraryContainer :: (Arbitrary a) => Gen (Container a)
arbitraryContainer = oneof 
  [ liftM Raw arbitrary
  , liftM List arbitrary ]

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (DB k v) where
  arbitrary = liftM (DB . Map.fromList) arbitrary

prop_set_get :: String -> String -> TestDB -> Bool
prop_set_get k v db = runDBT db $ do
  runTC $ Set k v
  liftM2 (==) (runTC $ Get k)
              (return $ BulkRep $ Raw v)

prop_get_get :: String -> String -> TestDB -> Bool
prop_get_get k v db = runDBT db $
  liftM2 (==) (runTC $ Get k) 
              (runTC $ Get k)

prop_set_append :: String -> String -> String -> TestDB -> Bool
prop_set_append k v a db = runDBT db $ do
  runTC $ Set k v
  l <- runTC $ Append k a
  r <- runTC $ Get k
  return $  r == (BulkRep $ Raw $ v ++ a) 
         && l == (IntRep $ length (v ++ a))

prop_set_exists :: String -> String -> TestDB -> Bool
prop_set_exists k v db = runDBT db $ do
  runTC $ Set k v
  liftM2 (==) (runTC $ Exists k) 
              (return $ IntRep 1)

prop_del_app_eq_set :: String -> String -> TestDB -> Bool
prop_del_app_eq_set k v db = runDBT db $ do
  runTC $ Del [k]
  runTC $ Append k v
  r1 <- runTC $ Get k
  runTC $ Set k v
  r2 <- runTC $ Get k
  return $ r1 == r2 

prop_set_del_exists :: String -> String -> TestDB -> Bool
prop_set_del_exists k v db = runDBT db $ do
  runTC $ Set k v
  runTC $ Del [k]
  liftM2 (==) (runTC $ Exists k) 
              (return $ IntRep 0)

prop_del_get :: String -> TestDB -> Bool
prop_del_get k db = runDBT db $ do
  runTC $ Del [k]
  liftM2 (==) (runTC $ Get k)
              (return NBulkRep)

newtype Checker a = Checker { unChecker :: WriterT [Result] IO a }
  deriving ( Monad, MonadWriter [Result], MonadIO )

runChecker :: Checker () -> IO [Result]
runChecker = liftM snd . runWriterT . unChecker

check :: Testable prop => prop -> Checker ()
check p = liftIO (quickCheckResult p) >>= tell . (:[])

checks :: Checker ()
checks = do
  check prop_set_get
  check prop_get_get
  check prop_set_append
  check prop_set_del_exists
  check prop_set_exists
  check prop_del_app_eq_set
  check prop_del_get

main :: IO ()
main = runChecker checks >>= \rs ->
  unless (all isSuccess rs) exitFailure
