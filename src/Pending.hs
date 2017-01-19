{-# LANGUAGE DeriveGeneric #-}

module Pending ( findPending ) where

import Ast

import Data.Tree

type PendingTree = Tree String

size :: PendingTree -> Int
size (Node _ []) = 1
size (Node _ (t @ (_:_))) = sum $ map size t

limitPendingTree :: Int -> PendingTree -> PendingTree
limitPendingTree _ (Node t []) = (Node t [])
limitPendingTree 1 (Node t sub) = (Node (t ++ " (" ++ (show $ sum $ map size sub) ++ " tasks)") [])
limitPendingTree n (Node t sub) = (Node t (map (limitPendingTree (n-1)) sub))

pendingJudgement :: Judgement -> [PendingTree]
pendingJudgement (Bonus (_, _)) = []
pendingJudgement (Judgement (Header (t, p, _), _, _, [])) | isInfinite p = [Node t []]
pendingJudgement (Judgement (Header (_, _, _), _, _, [])) = [] -- Not infinite
pendingJudgement (Judgement (Header (t, _, _), _, _, subJs @ (_:_))) =
  case (concatMap pendingJudgement subJs) of
    [] -> []
    sub  -> [Node t sub]

findPending :: Maybe Int -> [Judgement] -> Maybe(String)
findPending detailLevel js =
  case (concatMap pendingJudgement js) of
    [] -> Nothing
    t  -> 
      case detailLevel of
        Nothing  -> Just $ concatMap drawTree t
        (Just i) -> Just $ concatMap (drawTree . (limitPendingTree i)) t

