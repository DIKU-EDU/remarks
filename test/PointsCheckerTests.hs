{-# LANGUAGE DeriveGeneric #-}

module PointsCheckerTests ( allTests ) where

import Ast
import Parser
import PointsChecker
import Invalid

import Test.Tasty
import Test.Tasty.HUnit

checkPointsStr :: String -> [Either Invalid Judgement]
checkPointsStr s =
  case parseString s of
    Right js -> map checkPoints js
    Left _ -> []

posUnitTests :: TestTree
posUnitTests = testGroup "Positive Unit Tests"
  [ testCase "Lone header line" $
      checkPointsStr "# A: 0/0\n" @?=
        [ Right $
          Judgement (Header ("A", 0.0, 0.0), [], [], [])]
  , testCase "A couple same-depth header lines" $
      checkPointsStr "# A: 0/0\n# B: 0/0\n" @?=
        [ Right $
          Judgement (Header ("A", 0.0, 0.0), [], [], [])
        , Right $
          Judgement (Header ("B", 0.0, 0.0), [], [], [])
        ]
  , testCase "A simple hierarchy of headers" $
      checkPointsStr "# A: 0/0\n## B: 0/0\n" @?=
        [ Right $
          Judgement (Header ("A", 0.0, 0.0), [], [],
            [Judgement (Header ("B", 0.0, 0.0), [], [], [])])]
  , testCase "A couple simple hierarchies" $
      checkPointsStr "# A: 0/0\n## B: 0/0\n# C: 0/0\n" @?=
        [ Right $
          Judgement (Header ("A", 0.0, 0.0), [], [],
            [Judgement (Header ("B", 0.0, 0.0), [], [], [])])
        , Right $
          Judgement (Header ("C", 0.0, 0.0), [], [], [])
        ]
  ]

negUnitTests :: TestTree
negUnitTests = testGroup "Positive Unit Tests"
  [ testCase "Points exceed max points" $
      checkPointsStr "# A: 1/0\n" @?=
        [Left $ PointsExceedMaxPoints (Header ("A", 1.0, 0.0))]
  , testCase "Sub-judgement points don't sum up to points" $
      checkPointsStr "# A: 0/0\n## B: 1/0\n" @?=
        [Left $ BadSubJudgementPointsSum
          (Judgement (Header ("A", 0.0, 0.0), [], [],
            [Judgement (Header ("B", 1.0, 0.0), [], [], [])]))]
  , testCase "Sub-judgement max-points don't sum up to max-points" $
      checkPointsStr "# A: 0/0\n## B: 0/1\n" @?=
        [Left $ BadSubJudgementMaxPointsSum
          (Judgement (Header ("A", 0.0, 0.0), [], [],
            [Judgement (Header ("B", 0.0, 1.0), [], [], [])]))]
  , testCase "Single judgement with no points" $
      checkPointsStr "# A: /0\n" @?=
        [Left $ NoPointsInBottomJudgement
          (Judgement (Header ("A", 1/0, 0.0), [], [], []))]
  ]

allTests :: TestTree
allTests = testGroup "PointsChecker tests"
  [ posUnitTests, negUnitTests ]
