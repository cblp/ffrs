module Main where

import           System.IO.Temp (withSystemTempDirectory)
import           Test.Tasty (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit (testCase)

import qualified FF.Main as FF

main :: IO ()
main = defaultMain $ testGroup "" [testAddAndRetrieve]

testAddAndRetrieve :: TestTree
testAddAndRetrieve =
    testCase "add and retrieve" $
        withSystemTempDirectory "" $ \dir ->
            FF.main [] [("HOME", dir)]
