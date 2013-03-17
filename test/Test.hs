import System.Exit
import Test.QuickCheck
import Test.QuickCheck.Test
import Control.Monad

import Redish

instance Arbitrary (Container String) where
  arbitrary = arbitraryContainer

arbitraryContainer :: Gen (Container String)
arbitraryContainer = elements [
    Raw (arbitrary :: String)
  , List (arbitrary :: [String])
  ]

{-instance Arbitrary DB where -}
  {-arbitrary = sized arbitraryDB-}

{-arbitraryDB :: Int -> Gen DB-}
{-arbitraryDB 0 = emptyDB-}
{-arbitraryDB n = emptyDB-}

prop_lol :: Integer -> Bool
prop_lol a = a == (a+2)

prop_lul :: Integer -> Bool
prop_lul a = a == a

main = forM [
    prop_lol
  , prop_lul
  ] quickCheckResult >>= \rs -> 
    if (all isSuccess rs) then return () 
                          else exitFailure
