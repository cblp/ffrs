module Main where

import qualified Data.Map as Map
import           System.Environment (getEnvironment)
import           System.IO.Temp (withSystemTempDirectory)
import           System.Process (env, proc, readCreateProcess)
import           Test.Tasty (TestName, TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main = defaultMain $ testGroup "" [addAndRetrieve, firstRun]

addAndRetrieve :: TestTree
addAndRetrieve = testCaseRunFF "add and retrieve" $ \ff -> do
    add <- ff ["add", "example"]
    add @?= ["1. example"]
    list <- ff []
    list @?= ["1. example"]

firstRun :: TestTree
firstRun = testCaseRunFF "first run" $ \ff -> do
    list <- ff []
    list @?= []

type Proc = [String] -> IO [String]

testCaseRunFF :: TestName -> (Proc -> IO ()) -> TestTree
testCaseRunFF name action = testCase name $ do
    curEnv <- getEnvironment
    withSystemTempDirectory "ff.test." $ \dir -> do
        let env' = Map.assocs . Map.insert "HOME" dir $ Map.fromList curEnv
        let ff args = lines <$> readCreateProcess (proc "ff" args){env = Just env'} ""
        action ff
