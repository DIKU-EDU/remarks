{-# LANGUAGE DeriveGeneric #-}

module Pending ( findPending ) where

import Ast

import Data.Tree

type PendingTree = Tree String

pendingJudgement :: Judgement -> [PendingTree]
pendingJudgement (Bonus (_, _)) = []
pendingJudgement (Judgement (Header (t, p, _), _, _, [])) | isInfinite p = [Node t []]
pendingJudgement (Judgement (Header (_, _, _), _, _, [])) = [] -- Not infinite
pendingJudgement (Judgement (Header (t, _, _), _, _, subJs @ (_:_))) =
  case (concatMap pendingJudgement subJs) of
    [] -> []
    sub  -> [Node t sub]

findPending :: [Judgement] -> Maybe(String)
findPending js =
  case (concatMap pendingJudgement js) of
    [] -> Nothing
    t  -> Just $ concatMap drawTree t

