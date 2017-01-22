{-# LANGUAGE DeriveGeneric #-}

module Invalid ( reportInvalid, Invalid (..) ) where

import Ast
import PrettyPrinter

import Data.List (intersperse)
import Text.PrettyPrint.GenericPretty

data Invalid
  = PointsExceedMaxPoints Judgement
  | BadSubJudgementPointsSum Judgement
  | BadSubJudgementMaxPointsSum Judgement
  | NoPointsInBottomJudgement Judgement
  | PropertyNotFound String Judgement
  deriving (Eq, Show, Generic)

instance Out Invalid

reportInvalid :: Invalid -> String
reportInvalid (PointsExceedMaxPoints (j @ (Judgement ((Header (_, p, m)), _, _, _)))) =
    "The total points (" ++ ppPoints p ++ ") exceeded maximum (" ++ ppPoints m ++ ") in the judgement\n" ++
    reportStrippedJudgement j
reportInvalid (BadSubJudgementPointsSum (j @ (Judgement (Header (_, p, _), _, _, _)))) =
    "The sum of points (" ++ ppPoints p ++ ") in judgement is not the sum of sub-judgements\n" ++
    reportJudgement 0 j
reportInvalid (BadSubJudgementMaxPointsSum (j @ (Judgement (Header (_, _, m), _, _, _)))) =
    "The maximum points (" ++ ppPoints m ++ ") in judgement is not the sum of sub-judgements\n" ++
    reportJudgement 0 j
reportInvalid (NoPointsInBottomJudgement j) = 
  "No points reported in leaf-judgement\n" ++ reportStrippedJudgement j
reportInvalid (PropertyNotFound s j) = 
  "Property " ++ s ++ " not found in judgement\n" ++ reportJudgement 0 j
reportInvalid m = "Cannot parse error message\n" ++ show m ++ "\nPlease report this message to someone!"

-- The Bool indicates of sub-judgements should be included
reportJudgement :: Int -> Judgement -> String
reportJudgement d j | isLeafJ j = ppJ_d d (stripJ j)
reportJudgement 1 j | isNodeJ j = (ppJ_d 1 $ stripJ j) ++ "\n  ..."
reportJudgement 0 j | isNodeJ j = 
  (ppJ_d 0 $ stripJ j) ++ (concat $ intersperse "\n" (map (reportJudgement 1) (subJs j)))
reportJudgement _ _ = ""

reportStrippedJudgement :: Judgement -> String
reportStrippedJudgement j | isLeafJ j = reportJudgement 0 (stripJ j)
reportStrippedJudgement j | isNodeJ j = (reportJudgement 0 (stripJ j)) ++ "\n  ..."
reportStrippedJudgement _ = "" -- Bonus

subJs :: Judgement -> [Judgement]
subJs (Judgement (_, _, _, js)) = js
subJs _ = [] -- Bunus

stripJ :: Judgement -> Judgement
stripJ (Judgement (h, _, _, _)) = Judgement (h, [], [], [])
stripJ b = b -- Bunus

isLeafJ :: Judgement -> Bool
isLeafJ (Judgement (_, _, _, []))    = True
isLeafJ (Judgement (_, _, _, (_:_))) = False
isLeafJ (Bonus _)                    = False

isNodeJ :: Judgement -> Bool
isNodeJ (Judgement (_, _, _, []))    = False
isNodeJ (Judgement (_, _, _, (_:_))) = True
isNodeJ (Bonus _)                    = False
