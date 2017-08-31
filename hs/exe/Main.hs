module Main where

import           System.Environment (getEnvironment)

main :: IO ()
main = getEnvironment >>= print . lookup "HOME"
