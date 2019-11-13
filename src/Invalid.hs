{-# LANGUAGE DeriveGeneric #-}

module Invalid ( reportInvalid, Invalid (..) ) where

import Ast
import PrettyPrinter

import Data.List (intersperse)
import Text.PrettyPrint.GenericPretty

data Invalid
  = PointsExceedMaxPoints String Judgement
  | BadSubJudgementPointsSum String Judgement
  | BadSubJudgementMaxPointsSum String Judgement
  | NoPointsInBottomJudgement String Judgement
  | PropertyNotFound Int String Judgement [[(String, PropertyValue)]]
  | StringInputToArithFun String Judgement
  deriving (Eq, Show, Generic)

instance Out Invalid

reportInvalid :: Invalid -> String
reportInvalid (PointsExceedMaxPoints s (j @ (Judgement ((Header (_, p, m)), _, _, _)))) =
    "In " ++ s ++ " the total points (" ++ ppPoints p ++ ") exceeded maximum (" ++ ppPoints (Given m) ++ ") in the judgement\n" ++
    reportStrippedJudgement j
reportInvalid (BadSubJudgementPointsSum s (j @ (Judgement (Header (_, p, _), _, _, _)))) =
    "In " ++ s ++ " the sum of points (" ++ ppPoints p ++ ") in judgement is not the sum of sub-judgements\n" ++
    reportJudgement 0 j
reportInvalid (BadSubJudgementMaxPointsSum s (j @ (Judgement (Header (_, _, m), _, _, _)))) =
    "In " ++ s ++ " the maximum points (" ++ ppPoints (Given m) ++ ") in judgement is not the sum of sub-judgements\n" ++
    reportJudgement 0 j
reportInvalid (NoPointsInBottomJudgement s j) =
  "In " ++ s ++ " no points reported in leaf-judgement\n" ++ reportStrippedJudgement j
reportInvalid (PropertyNotFound i s _ env) =
  "Property " ++ show i ++ "." ++ s ++ " not found in judgement\n" ++ show env  --reportJudgement 0 j
reportInvalid (StringInputToArithFun s j) =
  "Property " ++ s ++ " contains a string value in judgement\n" ++ reportJudgement 0 j
reportInvalid m = "Cannot parse error message\n" ++ show m ++ "\nPlease report this message to someone!"

reportJudgement :: Int -> Judgement -> String
reportJudgement d j | isLeafJ j = ppJ_d d (stripJ j)
reportJudgement 1 j | isNodeJ j = (ppJ_d 1 $ stripJ j) ++ "\n  ..."
reportJudgement 0 j | isNodeJ j =
  (ppJ_d 0 $ stripJ j) ++ "\n" ++ (concat $ intersperse "\n" (map (reportJudgement 1) (subJs j)))
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
