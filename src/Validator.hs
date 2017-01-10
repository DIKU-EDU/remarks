{-# LANGUAGE DeriveGeneric #-}

module Validator ( validate ) where

import Ast

import Control.Monad ( forM_ )

import Text.PrettyPrint.GenericPretty

data Invalid
  = PointsExceedMaxPoints Header
  | BadSubJudgementPointsSum Judgement
  | BadSubJudgementMaxPointsSum Judgement
  deriving (Eq, Show, Generic)

instance Out Invalid

try :: Bool -> Invalid -> Either Invalid ()
try True = \_ -> Right ()
try False = Left

validate :: Judgement -> Either Invalid ()
validate j @ (Judgement (h @ (Header (_, p, maxP)), _, subjs)) = do
  try (p <= maxP)
    (PointsExceedMaxPoints h)
  try (sum (map points subjs) - p <= 0.01)
    (BadSubJudgementPointsSum j)
  try (sum (map maxPoints subjs) - maxP <= 0.01)
    (BadSubJudgementMaxPointsSum j)
  forM_ subjs validate

points :: Judgement -> Double
points (Judgement (Header (_, v, _), _, _)) = v

maxPoints :: Judgement -> Double
maxPoints (Judgement (Header (_, _, v), _, _)) = v
