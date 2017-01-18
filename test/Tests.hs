module Main where

import Test.Tasty

import qualified Parser.BlackBoxTests as PBBT
import qualified PointsCheckerTests as PCT

main :: IO ()
main = defaultMain $ testGroup "All Tests"
  [ PBBT.allTests, PCT.allTests ]
