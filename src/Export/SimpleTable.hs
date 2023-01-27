module Export.SimpleTable (simpleTableRemarks) where

import Ast

import Export.Generic

import Prelude hiding ((<>)) -- use (<>) from Text.PrettyPrint


simpleTableRemarks :: [Judgement] -> Table
simpleTableRemarks js = Rows $ tblHead:(map formatStart js)
  where
    tblHead = "Student":"Total / 100":(tail $ tableHead (head js))

tableHead :: Judgement -> Row
tableHead (j@(Bonus _)) = [getTitle j]
tableHead (Feedback _) = []
tableHead (j@(Judgement (_, _, _, js))) =
  ((getTitle j) ++ " / " ++ (getMaxPoints j)):(concatMap tableHead js)

formatStart :: Judgement -> Row
formatStart (Bonus _) = []
formatStart (Feedback _) = []
formatStart (j@(Judgement (_, _, _, js))) =
  (getTitle j):(getTotal j):r1
  -- [(getTitle j):(getTotal j):r1]
  where
    r1 = concatMap formatJudgement js

formatJudgement :: Judgement -> Row
formatJudgement (j@(Bonus (_, _, _))) = [getTotal j]
formatJudgement (Feedback _) = []
-- formatJudgement (j @ (Judgement (Header(_,p,_), _, cs, []))) =
--   [getTotal j]
formatJudgement (j@(Judgement (_, _, _, js))) =
  (getTotal j):r1
  where
    r1 = concatMap formatJudgement js


