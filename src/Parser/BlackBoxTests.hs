{-# LANGUAGE DeriveGeneric #-}

module Parser.BlackBoxTests where

import Ast
import Parser

import Test.Tasty
import Test.Tasty.HUnit
-- import Test.Tasty.QuickCheck
import Test.Tasty.Golden

import Text.PrettyPrint.GenericPretty

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "Lone header line" $
      parseString "# A: 0/0\n" @?=
        Right [Judgement (Header ("A",0.0,0.0),[])]
  , testCase "A couple same-depth header lines" $
      parseString "# A: 0/0\n# B: 0/0\n" @?=
        Right
          [ Judgement (Header ("A",0.0,0.0),[])
          , Judgement (Header ("B",0.0,0.0),[])
          ]
  ]

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
