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
  , testCase "Lone header line (no line break at eof)" $
      parseString "# A: 0/0" @?=
        Right
          [ Judgement (Header ("A",Given 0,0), [], [], [])
          ]
  , testCase "A couple same-depth header lines" $
      parseString "# A: 0/0\n# B: 0/0\n" @?=
        Right
          [ Judgement (Header ("A",Given 0,0), [], [], [])
          , Judgement (Header ("B",Given 0,0), [], [], [])
          ]
  , testCase "A couple same-depth header lines (no line break at eof)" $
      parseString "# A: 0/0\n# B: 0/0" @?=
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
  , testCase "A simple hierarchy of headers (no line break at eof)" $
      parseString "# A: 0/0\n## B: 0/0" @?=
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
  , testCase "Simple bonus" $
      parseString "# Bonus: +5\n" @?=
        Right [Bonus (500, [], [])]
  , testCase "Simple bonus (no line break at eof)" $
      parseString "# Bonus: +5" @?=
        Right [Bonus (500, [], [])]
  , testCase "Bonus with props" $
      parseString "# Bonus: +5\n  :x:y\n" @?=
        Right [Bonus (500,[Property ("x",Value "y")],[])]
  , testCase "Bonus with props (no line break at eof)" $
      parseString "# Bonus: +5\n  :x:y" @?=
        Right [Bonus (500,[Property ("x",Value "y")],[])]
  , testCase "Judgement with comments (no line break at eof)" $
      parseString "# A: 5/10\n  - Bad indentation\n    I'd say" @?=
        Right
          [
            Judgement (Header ("A",Given 500,1000),[],[
              Comment (Negative,[
                CommentStr "Bad indentation",
                CommentStr "I'd say"
              ])],[])
          ]
  , testCase "Another judgement with comments (no line break at eof)" $
      parseString "# A: 5/10\n  - Bad indentation\n  + Otherwise, OK" @?=
        Right
          [
            Judgement (Header ("A",Given 500,1000),[],[
              Comment (Negative,[CommentStr "Bad indentation"]),
              Comment (Positive,[CommentStr "Otherwise, OK"])
            ],[])
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
