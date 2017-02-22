module Export.ResultsTable (resultsTableRemarks) where

import Ast

import Export.Generic

resultsTableRemarks :: [Judgement] -> Table 
resultsTableRemarks js = Rows $ tblHead:(concatMap formatStart js)
  where
    tblHead = "Student":"Total / 100":(tail $ tableHead (head js))

tableHead :: Judgement -> Row
tableHead (j @ (Bonus _)) = [getTitle j]
tableHead (j @ (Judgement (_, _, _, js))) =
  ((getTitle j) ++ " / " ++ (getMaxPoints j)):(concatMap tableHead js)

formatStart :: Judgement -> [Row]
formatStart (Bonus _) = []
formatStart (j @ (Judgement (_, _, _, js))) =
  [(getTitle j):(getTotal j):(concatMap formatJudgement js)]

formatJudgement :: Judgement -> Row
formatJudgement (j @ (Bonus (_, _, _))) = [getTotal j]
formatJudgement (j @ (Judgement (_, _, _, js))) =
  (getTotal j):(concatMap formatJudgement js)
