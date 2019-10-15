module Export.HtmlTable (htmlTableRemarks) where

import Ast

import Export.Generic
import PrettyPrinter

import Data.List (intersperse)


htmlTableRemarks :: [Judgement] -> Table 
htmlTableRemarks js = Rows $ tblHead:(concatMap formatStart js)
  where
    tblHead = "Student":"Total / 100":(tail $ tableHead (head js))

tableHead :: Judgement -> Row
tableHead (j @ (Bonus _)) = [getTitle j]
tableHead (Feedback _) = []
tableHead (j @ (Judgement (_, _, _, js))) =
  ((getTitle j) ++ " / " ++ (getMaxPoints j)):(concatMap tableHead js)

formatStart :: Judgement -> [Row]
formatStart (Bonus _) = []
formatStart (Feedback _) = []
formatStart (j @ (Judgement (_, _, cs, js))) =
  [(getTitle j):(getTotal j):r1,"":(formatComments cs):r2]
  -- [(getTitle j):(getTotal j):r1]
  where
    (r1, r2) = concatUnzipMap formatJudgement js

formatJudgement :: Judgement -> (Row, Row)
formatJudgement (j @ (Bonus (_, _, cs))) = ([getTotal j], [formatComments cs])
formatJudgement (Feedback _) = ([], [])
formatJudgement (j @ (Judgement (_, _, cs, js))) =
  ((getTotal j):r1, (formatComments cs):r2)
  where
    (r1, r2) = concatUnzipMap formatJudgement js

concatUnzipMap :: (a -> ([b],[c])) -> [a] -> ([b],[c])
concatUnzipMap f l =
  (concat c1, concat c2)
  where (c1, c2) = unzip $ map f l

formatComments :: [Comment] -> String
formatComments cs = concat $ intersperse "<br>" $ map (\x -> "" ++ ppComment x) cs


