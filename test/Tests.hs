module Main where

import Test.Tasty

import qualified Parser.BlackBoxTests as PBBT
import qualified ValidatorTests as VT

main :: IO ()
main = defaultMain $ testGroup "All Tests"
  [ PBBT.allTests, VT.allTests ]
