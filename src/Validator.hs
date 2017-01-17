{-# LANGUAGE DeriveGeneric #-}

module Validator ( validate, Invalid(..) ) where

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

infix 4 ~=
(~=) :: Double -> Double -> Bool
x ~= y = abs (x - y) <= 0.01

validate :: Judgement -> Either Invalid ()
validate j @ (Judgement (h @ (Header (_, p, maxP)), _, subjs)) = do
  try (p <= maxP)
    (PointsExceedMaxPoints h)
  case subjs of
    [] -> return ()
    _ -> do
      try ((sum $ map points subjs) ~= p)
        (BadSubJudgementPointsSum j)
      try ((sum $ map maxPoints subjs) ~= maxP)
        (BadSubJudgementMaxPointsSum j)
      forM_ subjs validate

points :: Judgement -> Double
points (Judgement (Header (_, v, _), _, _)) = v

maxPoints :: Judgement -> Double
maxPoints (Judgement (Header (_, _, v), _, _)) = v
