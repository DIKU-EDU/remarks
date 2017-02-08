module PointsChecker ( checkPoints ) where

import Ast
import Invalid

try :: Bool -> Invalid -> Either Invalid ()
try True = \_ -> Right ()
try False = Left

infix 4 ~=
(~=) :: Double -> Double -> Bool
x ~= y = abs (x - y) <= 0.01

checkPoints :: Judgement -> Either Invalid Judgement
checkPoints (j@(Judgement (Header (t, _, _), _, _, _))) = checkPointsJ t j
checkPoints b = pure b

checkPointsSubJs :: String -> Judgement -> Either Invalid Judgement
checkPointsSubJs s (Judgement (Header (t, _, maxP), prop, cs, subJs)) = do
  newSubJs <- mapM (checkPointsJ s) subJs
  let newP = sum $ map points newSubJs
  pure $ Judgement (Header (t, newP, maxP), prop, cs, newSubJs)
checkPointsSubJs _ j = pure j

checkPointsJ :: String ->  Judgement -> Either Invalid Judgement
checkPointsJ s (j @ (Judgement ((Header (_, p, _)), _, _, []))) | isInfinite p =
  Left $ NoPointsInBottomJudgement s j
checkPointsJ s (j @ (Judgement (Header (_, p, maxP), _, _, subJs @ (_:_)))) | isInfinite p = do
  try ((sum $ map maxPoints subJs) ~= maxP)
    (BadSubJudgementMaxPointsSum s j)
  checkPointsSubJs s j
checkPointsJ s (j @ (Judgement (Header (_, p, maxP), _, _, subJs))) = do
  try (p <= maxP)
    (PointsExceedMaxPoints s j)
  case subJs of
    [] -> pure j
    _ -> do
      try ((sum $ map points subJs) ~= p)
        (BadSubJudgementPointsSum s j)
      try ((sum $ map maxPoints subJs) ~= maxP)
        (BadSubJudgementMaxPointsSum s j)
      checkPointsSubJs s j
checkPointsJ _ j @ (Bonus _) = pure j

points :: Judgement -> Double
points (Bonus (v, _, _)) = v
points (Judgement (Header (_, v, _), _, _, [])) | isInfinite v = 0
points (Judgement (Header (_, v, _), _, _, _)) = v

maxPoints :: Judgement -> Double
maxPoints (Bonus _) = 0.0
maxPoints (Judgement (Header (_, _, v), _, _, _)) = v
