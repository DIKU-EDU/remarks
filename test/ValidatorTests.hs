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

validateStr :: String -> [Either Invalid Judgement]
validateStr s =
  case parseString s of
    Right js -> map validate js
    Left e -> []

posUnitTests :: TestTree
posUnitTests = testGroup "Positive Unit Tests"
  [ testCase "Lone header line" $
      validateStr "# A: 0/0\n" @?=
        [ Right $
          Judgement (Header ("A", 0.0, 0.0), [], [])]
  , testCase "A couple same-depth header lines" $
      validateStr "# A: 0/0\n# B: 0/0\n" @?=
        [ Right $
          Judgement (Header ("A", 0.0, 0.0), [], [])
        , Right $
          Judgement (Header ("B", 0.0, 0.0), [], [])
        ]
  , testCase "A simple hierarchy of headers" $
      validateStr "# A: 0/0\n## B: 0/0\n" @?=
        [ Right $
          Judgement (Header ("A", 0.0, 0.0), [],
            [Judgement (Header ("B", 0.0, 0.0), [], [])])]
  , testCase "A couple simple hierarchies" $
      validateStr "# A: 0/0\n## B: 0/0\n# C: 0/0\n" @?=
        [ Right $
          Judgement (Header ("A", 0.0, 0.0), [],
            [Judgement (Header ("B", 0.0, 0.0), [], [])])
        , Right $
          Judgement (Header ("C", 0.0, 0.0), [], [])
        ]
  ]

negUnitTests :: TestTree
negUnitTests = testGroup "Positive Unit Tests"
  [ testCase "Points exceed max points" $
      validateStr "# A: 1/0\n" @?=
        [Left $ PointsExceedMaxPoints (Header ("A", 1.0, 0.0))]
  , testCase "Sub-judgement points don't sum up to points" $
      validateStr "# A: 0/0\n## B: 1/0\n" @?=
        [Left $ BadSubJudgementPointsSum
          (Judgement (Header ("A", 0.0, 0.0), [],
            [Judgement (Header ("B", 1.0, 0.0), [], [])]))]
  , testCase "Sub-judgement max-points don't sum up to max-points" $
      validateStr "# A: 0/0\n## B: 0/1\n" @?=
        [Left $ BadSubJudgementMaxPointsSum
          (Judgement (Header ("A", 0.0, 0.0), [],
            [Judgement (Header ("B", 0.0, 1.0), [], [])]))]
  , testCase "Single judgement with no points" $
      validateStr "# A: /0\n" @?=
        [Left $ NoPointsInBottomJudgement
          (Judgement (Header ("A", 1/0, 0.0), [], []))]
  ]

qcTests :: TestTree
qcTests = testGroup "QuickCheck tests" []

goldenTests :: TestTree
goldenTests = testGroup "Golden tests" []

allTests :: TestTree
allTests = testGroup "Validator tests"
  [ posUnitTests, negUnitTests, qcTests, goldenTests ]
