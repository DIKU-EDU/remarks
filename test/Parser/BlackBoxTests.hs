{-# LANGUAGE DeriveGeneric #-}

module Parser.BlackBoxTests ( allTests ) where

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
        Right
          [ Judgement (Header ("A",Given 0,0), [], [], [])
          ]
  , testCase "A couple same-depth header lines" $
      parseString "# A: 0/0\n# B: 0/0\n" @?=
        Right
          [ Judgement (Header ("A",Given 0,0), [], [], [])
          , Judgement (Header ("B",Given 0,0), [], [], [])
          ]
  , testCase "A simple hierarchy of headers" $
      parseString "# A: 0/0\n## B: 0/0\n" @?=
        Right
          [ Judgement (Header ("A",Given 0,0), [], [],
            [ Judgement (Header ("B",Given 0,0), [], [], [])
            ])
          ]
  , testCase "A couple simple hierarchies" $
      parseString "# A: 0/0\n## B: 0/0\n# C: 0/0\n" @?=
        Right
          [ Judgement (Header ("A",Given 0,0), [], [],
            [ Judgement (Header ("B",Given 0,0), [], [], [])
            ])
          , Judgement (Header ("C",Given 0,0), [], [], [])
          ]
  ]

qcTests :: TestTree
qcTests = testGroup "QuickCheck tests" []

golden' :: String -> TestTree
golden' basename =
  goldenVsFile name fref fout $ do
    p <- parseFile fin
    let ps = case p of
              Left e -> show e
              Right p' -> pretty p'
    writeFile fout $ ps ++ "\n"
  where
    prefix = "test/golden/"
    name = basename ++ ".mrk"
    fin = prefix ++ name
    fref = prefix ++ basename ++ ".ref"
    fout = prefix ++ basename ++ ".out"

goldenTests :: TestTree
goldenTests = testGroup "Golden tests"
  [ golden' "deep-judgement"
  , golden' "some-top-level-judgements"
  , golden' "some-judgements"
  ]

allTests :: TestTree
allTests = testGroup "Black-box parser tests"
  [ unitTests, qcTests, goldenTests ]
