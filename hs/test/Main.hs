module Main where

import qualified Data.Map as Map
import           System.Environment (getEnvironment)
import           System.IO.Temp (withSystemTempDirectory)
import           System.Process (env, proc, readCreateProcess)
import           Test.Tasty (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit (testCase)

main :: IO ()
main = defaultMain $ testGroup "" [testAddAndRetrieve]

testAddAndRetrieve :: TestTree
testAddAndRetrieve =
    testCase "add and retrieve" $ do
        curEnv <- getEnvironment
        withSystemTempDirectory "ff.test." $ \dir -> do
            let env' = Map.assocs . Map.insert "HOME" dir $ Map.fromList curEnv
            let ff args = (proc "ff" args){env = Just env'}
            readCreateProcess (ff ["add"]) "" >>= putStrLn
