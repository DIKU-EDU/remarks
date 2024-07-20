{-# LANGUAGE DeriveGeneric #-}

module PointsCheckerTests (allTests) where

import Ast
import Invalid
import Parser
import PointsChecker
import Test.Tasty
import Test.Tasty.HUnit

checkPointsStr :: String -> [Either Invalid Judgement]
checkPointsStr s =
  case parseString s of
    Right js -> map checkPoints js
    Left _ -> []

posUnitTests :: TestTree
posUnitTests =
  testGroup
    "Positive Unit Tests"
    [ testCase "Lone header line" $
        checkPointsStr "# A: 0/0\n"
          @?= [ Right $
                  Judgement (Header ("A", Given 0, 0), [], [], [])
              ],
      testCase "A couple same-depth header lines" $
        checkPointsStr "# A: 0/0\n# B: 0/0\n"
          @?= [ Right $
                  Judgement (Header ("A", Given 0, 0), [], [], []),
                Right $
                  Judgement (Header ("B", Given 0, 0), [], [], [])
              ],
      testCase "A simple hierarchy of headers" $
        checkPointsStr "# A: 0/0\n## B: 0/0\n"
          @?= [ Right $
                  Judgement
                    ( Header ("A", Given 0, 0),
                      [],
                      [],
                      [Judgement (Header ("B", Given 0, 0), [], [], [])]
                    )
              ],
      testCase "A couple simple hierarchies" $
        checkPointsStr "# A: 0/0\n## B: 0/0\n# C: 0/0\n"
          @?= [ Right $
                  Judgement
                    ( Header ("A", Given 0, 0),
                      [],
                      [],
                      [Judgement (Header ("B", Given 0, 0), [], [], [])]
                    ),
                Right $
                  Judgement (Header ("C", Given 0, 0), [], [], [])
              ]
    ]

negUnitTests :: TestTree
negUnitTests =
  testGroup
    "Negative Unit Tests"
    [ testCase "Points exceed max points" $
        checkPointsStr "# A: 1/0\n"
          @?= [ Left $
                  PointsExceedMaxPoints
                    "A"
                    (Judgement (Header ("A", Given 100, 0), [], [], []))
              ],
      testCase "Sub-judgement points don't sum up to points" $
        checkPointsStr "# A: 0/0\n## B: 1/0\n"
          @?= [ Left $
                  BadSubJudgementPointsSum
                    "A"
                    ( Judgement
                        ( Header ("A", Given 0, 0),
                          [],
                          [],
                          [Judgement (Header ("B", Given 100, 0), [], [], [])]
                        )
                    )
              ],
      testCase "Sub-judgement max-points don't sum up to max-points" $
        checkPointsStr "# A: 0/0\n## B: 0/1\n"
          @?= [ Left $
                  BadSubJudgementMaxPointsSum
                    "A"
                    ( Judgement
                        ( Header ("A", Given 0, 0),
                          [],
                          [],
                          [Judgement (Header ("B", Given 0, 100), [], [], [])]
                        )
                    )
              ],
      testCase "Single judgement with no points" $
        checkPointsStr "# A: /0\n"
          @?= [ Left $
                  NoPointsInBottomJudgement
                    "A"
                    (Judgement (Header ("A", NotGiven, 0), [], [], []))
              ]
    ]

allTests :: TestTree
allTests =
  testGroup
    "PointsChecker tests"
    [posUnitTests, negUnitTests]
