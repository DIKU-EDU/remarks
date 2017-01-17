{-# LANGUAGE DeriveGeneric #-}

module Validator ( validate, Invalid(..) ) where

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

validateSubJs :: Judgement -> Either Invalid Judgement
validateSubJs (Judgement (h @ (Header (t, _, maxP)), cs, subJs)) = do
  newSubJs <- mapM validate subJs
  let newP = sum $ map points newSubJs
  pure $ Judgement (Header (t, newP, maxP), cs, newSubJs)
validateSubJs j = pure j

validate :: Judgement -> Either Invalid Judgement
validate j @ (Judgement (h @ (Header (_, p, maxP)), _, [])) | isInfinite p = do
  Left $ NoPointsInBottomJudgement j
validate j @ (Judgement (h @ (Header (_, p, maxP)), _, subJs @ (_:_))) | isInfinite p = do
  try ((sum $ map maxPoints subJs) ~= maxP)
    (BadSubJudgementMaxPointsSum j)
  validateSubJs j
validate j @ (Judgement (h @ (Header (_, p, maxP)), _, subJs)) = do
  try (p <= maxP)
    (PointsExceedMaxPoints h)
  case subJs of
    [] -> pure j
    _ -> do
      try ((sum $ map points subJs) ~= p)
        (BadSubJudgementPointsSum j)
      try ((sum $ map maxPoints subJs) ~= maxP)
        (BadSubJudgementMaxPointsSum j)
      validateSubJs j
validate j = pure j

points :: Judgement -> Double
points (Bonus (v, _)) = v
points (Judgement (Header (_, v, _), _, _)) = v

maxPoints :: Judgement -> Double
maxPoints (Bonus _) = 0.0
maxPoints (Judgement (Header (_, _, v), _, _)) = v
