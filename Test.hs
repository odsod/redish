{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import System.Exit
import Test.QuickCheck
import Test.QuickCheck.Test
import Control.Monad
import Control.Applicative
import Control.Monad.Writer
import Data.Monoid
import qualified Data.Map as Map

import Redish

instance (Arbitrary a) => Arbitrary (Container a) where
  arbitrary = arbitraryContainer

arbitraryContainer :: (Arbitrary a) => Gen (Container a)
arbitraryContainer = oneof [ 
    liftM Raw arbitrary
  , liftM List arbitrary ]

troll = sample (arbitraryContainer :: Gen(Container String))

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (DB k v) where
  arbitrary = liftM (DB . Map.fromList) arbitrary

{-prop_set_get :: (Monoid v, Sized v, Eq v) => k -> v -> DB k v -> Bool-}
prop_set_get :: String -> String -> DB String String -> Bool
prop_set_get k v db = let (_, db') = runCommand db (Set k v) 
                          (r, _) = runCommand db' (Get k) 
                          in r == (Val $ Raw v)

prop_get_get :: String -> String -> DB String String -> Bool
prop_get_get k v db = let (r1, db') = runCommand db (Get k)
                          (r2, _) = runCommand db' (Get k)
                          in r1 == r2

prop_set_append :: String -> String -> String -> DB String String -> Bool
prop_set_append k v a db = let (_, db') = runCommand db (Set k v)
                               (l, db'') = runCommand db' (Append k a)
                               (r, _) = runCommand db'' (Get k)
                               in r == (Val $ Raw $ v ++ a) && l == (Len $ length (v ++ a))

newtype Checker a = Checker { unChecker :: WriterT [Result] IO a }
  deriving ( Monad, MonadWriter [Result], MonadIO )

runChecker :: Checker () -> IO ([Result])
runChecker = liftM snd . runWriterT . unChecker

check :: Testable prop => prop -> Checker ()
check p = liftIO (quickCheckResult p) >>= tell . (:[])

checks = do
  check prop_set_get
  check prop_get_get
  check prop_set_append

main :: IO ()
main = runChecker checks >>= \rs ->
  if all isSuccess rs then return () else exitFailure
