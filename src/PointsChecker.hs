module PointsChecker ( checkPoints, validPoints ) where

import Ast
import Invalid

try :: Bool -> Invalid -> Either Invalid ()
try True = \_ -> Right ()
try False = Left

checkPoints :: Judgement -> Either Invalid Judgement
checkPoints (j@(Judgement (Header (t, _, _), _, _, _))) = checkPointsJ t j
checkPoints b = pure b

checkPointsSubJs :: String -> Judgement -> Either Invalid Judgement
checkPointsSubJs s (Judgement (Header (t, _, maxP), prop, cs, subJs)) = do
  newSubJs <- mapM (checkPointsJ s) subJs
  let newP = sum $ map points newSubJs
  pure $ Judgement (Header (t, Given newP, maxP), prop, cs, newSubJs)
checkPointsSubJs _ j = pure j

checkPointsJ :: String ->  Judgement -> Either Invalid Judgement
checkPointsJ s (j@(Judgement ((Header (_, NotGiven, _)), _, _, []))) =
  Left $ NoPointsInBottomJudgement s j
checkPointsJ s (j@(Judgement (Header (_, NotGiven, maxP), _, _, subJs@(_:_)))) = do
  try ((sum $ map maxPoints subJs) == maxP)
    (BadSubJudgementMaxPointsSum s j)
  checkPointsSubJs s j
checkPointsJ s (j@ (Judgement (Header (_, NotMade, maxP), _, _, subJs))) = do
  case subJs of
    [] -> pure j
    _ -> do
      try ((sum $ map points subJs) == 0)
        (BadSubJudgementPointsSum s j)
      try ((sum $ map maxPoints subJs) == maxP)
        (BadSubJudgementMaxPointsSum s j)
      checkPointsSubJs s j
checkPointsJ s (j@(Judgement (Header (_, (Given p), maxP), _, _, subJs))) = do
  try (p <= maxP)
    (PointsExceedMaxPoints s j)
  case subJs of
    [] -> pure j
    _ -> do
      try ((sum $ map points subJs) == p)
        (BadSubJudgementPointsSum s j)
      try ((sum $ map maxPoints subJs) == maxP)
        (BadSubJudgementMaxPointsSum s j)
      checkPointsSubJs s j
checkPointsJ _ j@(Bonus _) = pure j
checkPointsJ _ j@(Feedback _) = pure j

validPoints :: Judgement -> Either Invalid Judgement
validPoints (j@(Judgement (Header (t, _, _), _, _, _))) = validPointsJ t j
validPoints b = pure b

validPointsSubJs :: String -> Judgement -> Either Invalid Judgement
validPointsSubJs s (Judgement (Header (t, _, maxP), prop, cs, subJs)) = do
  newSubJs <- mapM (validPointsJ s) subJs
  let newP = sum $ map points newSubJs
  pure $ Judgement (Header (t, Given newP, maxP), prop, cs, newSubJs)
validPointsSubJs _ j = pure j

validPointsJ :: String ->  Judgement -> Either Invalid Judgement
validPointsJ _ (j@(Judgement ((Header (_, NotGiven, _)), _, _, []))) = pure j
validPointsJ s (j@(Judgement (Header (_, NotGiven, maxP), _, _, subJs@(_:_)))) = do
  try ((sum $ map maxPoints subJs) == maxP)
    (BadSubJudgementMaxPointsSum s j)
  validPointsSubJs s j
validPointsJ s (j@ (Judgement (Header (_, NotMade, maxP), _, _, subJs))) = do
  case subJs of
    [] -> pure j
    _ -> do
      try ((sum $ map points subJs) == 0)
        (BadSubJudgementPointsSum s j)
      try ((sum $ map maxPoints subJs) == maxP)
        (BadSubJudgementMaxPointsSum s j)
      validPointsSubJs s j
validPointsJ s (j@(Judgement (Header (_, (Given p), maxP), _, _, subJs))) = do
  try (p <= maxP)
    (PointsExceedMaxPoints s j)
  case subJs of
    [] -> pure j
    _ -> do
      try ((sum $ map points subJs) == p)
        (BadSubJudgementPointsSum s j)
      try ((sum $ map maxPoints subJs) == maxP)
        (BadSubJudgementMaxPointsSum s j)
      validPointsSubJs s j
validPointsJ _ j@(Bonus _) = pure j
validPointsJ _ j@(Feedback _) = pure j

points :: Judgement -> Int
points (Bonus (v, _, _)) = v
points (Feedback _) = 0
points (Judgement (Header (_, NotGiven, _), _, _, _)) = 0
points (Judgement (Header (_, (Given v), _), _, _, _)) = v
points (Judgement (Header (_, NotMade, _), _, _, _)) = 0

maxPoints :: Judgement -> Int
maxPoints (Bonus _) = 0
maxPoints (Feedback _) = 0
maxPoints (Judgement (Header (_, _, v), _, _, _)) = v
