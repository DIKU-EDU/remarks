module Main where

import qualified Parser.BlackBoxTests as PBBT
import qualified PointsCheckerTests as PCT
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "All Tests"
      [PBBT.allTests, PCT.allTests]
