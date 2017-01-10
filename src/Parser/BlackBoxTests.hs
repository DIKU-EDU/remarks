{-# LANGUAGE DeriveGeneric #-}

module Parser.BlackBoxTests where

-- import Ast
import Parser

import Test.Tasty
-- import Test.Tasty.HUnit
-- import Test.Tasty.QuickCheck
import Test.Tasty.Golden

import Text.PrettyPrint.GenericPretty

unitTests :: TestTree
unitTests = testGroup "Unit tests" []

qcTests :: TestTree
qcTests = testGroup "QuickCheck tests" []

golden' :: String -> TestTree
golden' basename =
  goldenVsFile name fref fout $ do
    p <- parseFile fin
    writeFile fout $ (pretty p) ++ "\n"
  where
    prefix = "test/golden/"
    name = basename ++ ".mrk"
    fin = prefix ++ name
    fref = prefix ++ basename ++ ".ref"
    fout = prefix ++ basename ++ ".out"

goldenTests :: TestTree
goldenTests = testGroup "Golden tests"
  [ golden' "deep-judgement"
  ]

main :: IO ()
main = defaultMain $ testGroup "White-box parser tests"
  [ unitTests, qcTests, goldenTests ]
