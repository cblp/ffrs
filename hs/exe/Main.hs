module Main where

import           System.Environment (getEnvironment)

main :: IO ()
main = getEnvironment >>= print . filter (\(x, _) -> x == "HOME")
