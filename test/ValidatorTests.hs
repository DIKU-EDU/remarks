{-# LANGUAGE DeriveGeneric #-}

module ValidatorTests ( allTests ) where

import Ast
import Parser
import Validator

import Test.Tasty
import Test.Tasty.HUnit
-- import Test.Tasty.QuickCheck
import Test.Tasty.Golden

import Text.PrettyPrint.GenericPretty

validateStr :: String -> [Either Invalid ()]
validateStr s =
  case parseString s of
    Right js -> map validate js
    Left e -> []

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "Lone header line" $
      validateStr "# A: 0/0\n" @?=
        [Right ()]
  , testCase "A couple same-depth header lines" $
      validateStr "# A: 0/0\n# B: 0/0\n" @?=
        [Right (), Right ()]
  , testCase "A simple hierarchy of headers" $
      validateStr "# A: 0/0\n## B: 0/0\n" @?=
        [Right ()]
  , testCase "A couple simple hierarchies" $
      validateStr "# A: 0/0\n## B: 0/0\n# C: 0/0\n" @?=
        [Right (), Right ()]
  ]

qcTests :: TestTree
qcTests = testGroup "QuickCheck tests" []

goldenTests :: TestTree
goldenTests = testGroup "Golden tests" []

allTests :: TestTree
allTests = testGroup "Validator tests"
  [ unitTests, qcTests, goldenTests ]
