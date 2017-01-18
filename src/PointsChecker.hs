{-# LANGUAGE DeriveGeneric #-}

module PointsChecker ( checkPoints, Invalid(..) ) where

import Ast

import Control.Monad ( forM_ )

import Text.PrettyPrint.GenericPretty

data Invalid
  = PointsExceedMaxPoints Header
  | BadSubJudgementPointsSum Judgement
  | BadSubJudgementMaxPointsSum Judgement
  | NoPointsInBottomJudgement Judgement
  deriving (Eq, Show, Generic)

instance Out Invalid

try :: Bool -> Invalid -> Either Invalid ()
try True = \_ -> Right ()
try False = Left

infix 4 ~=
(~=) :: Double -> Double -> Bool
x ~= y = abs (x - y) <= 0.01

checkPointsSubJs :: Judgement -> Either Invalid Judgement
checkPointsSubJs (Judgement (h @ (Header (t, _, maxP)), prop, cs, subJs)) = do
  newSubJs <- mapM checkPoints subJs
  let newP = sum $ map points newSubJs
  pure $ Judgement (Header (t, newP, maxP), prop, cs, newSubJs)
checkPointsSubJs j = pure j

checkPoints :: Judgement -> Either Invalid Judgement
checkPoints j @ (Judgement (h @ (Header (_, p, maxP)), _, _, [])) | isInfinite p = do
  Left $ NoPointsInBottomJudgement j
checkPoints j @ (Judgement (h @ (Header (_, p, maxP)), _, _, subJs @ (_:_))) | isInfinite p = do
  try ((sum $ map maxPoints subJs) ~= maxP)
    (BadSubJudgementMaxPointsSum j)
  checkPointsSubJs j
checkPoints j @ (Judgement (h @ (Header (_, p, maxP)), _, _, subJs)) = do
  try (p <= maxP)
    (PointsExceedMaxPoints h)
  case subJs of
    [] -> pure j
    _ -> do
      try ((sum $ map points subJs) ~= p)
        (BadSubJudgementPointsSum j)
      try ((sum $ map maxPoints subJs) ~= maxP)
        (BadSubJudgementMaxPointsSum j)
      checkPointsSubJs j
checkPoints j = pure j

points :: Judgement -> Double
points (Bonus (v, _)) = v
points (Judgement (Header (_, v, _), _, _, _)) = v

maxPoints :: Judgement -> Double
maxPoints (Bonus _) = 0.0
maxPoints (Judgement (Header (_, _, v), _, _, _)) = v
