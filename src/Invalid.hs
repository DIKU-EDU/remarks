{-# LANGUAGE DeriveGeneric #-}

module Invalid where

import Ast

import Text.PrettyPrint.GenericPretty

data Invalid
  = PointsExceedMaxPoints Header
  | BadSubJudgementPointsSum Judgement
  | BadSubJudgementMaxPointsSum Judgement
  | NoPointsInBottomJudgement Judgement
  | PropertyNotFound String
  deriving (Eq, Show, Generic)

instance Out Invalid

