-- | tasty and doctest are used to orchestrate all testing.
module Main where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Instances ()
import Test.Tasty.HUnit as HU
import Test.DocTest
import Data.List

main :: IO ()
main = defaultMain tests

doctestModule :: String -> IO ()
doctestModule modulePath = doctest ["-isrc", "src/" ++ modulePath]

tests :: TestTree
tests = testGroup "Tests" [ ldap
                          ]

ldap :: TestTree
ldap = testGroup "LDAP" [ doctests
                        , hunits
                        ] where
        doctests = HU.testCase "doctests" $ doctestModule "CSH/LDAP.hs"
