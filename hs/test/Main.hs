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
    add1 <- ff ["add", "example"]
    add1 @?= ""
    list1 <- ff []
    list1 @?= "1 example"
    add2 <- ff ["add", "example"]
    add2 @?= ""
    list2 <- ff []
    list2 @?= unlines ["1 example", "2 example"]

firstRun :: TestTree
firstRun = testCaseRunFF "first run" $ \ff -> do
    list <- ff []
    list @?= ""

type Proc = [String] -> IO String

testCaseRunFF :: TestName -> (Proc -> IO ()) -> TestTree
testCaseRunFF name action = testCase name $ do
    curEnv <- getEnvironment
    withSystemTempDirectory "ff.test." $ \dir -> do
        let env' = Map.assocs . Map.insert "HOME" dir $ Map.fromList curEnv
        let ff args = readCreateProcess (proc "ff" args){env = Just env'} ""
        action ff
