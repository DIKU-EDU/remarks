{-# LANGUAGE DeriveGeneric #-}

module Pending ( findPending ) where

import Ast

import Data.Tree
import Text.PrettyPrint

type PendingTree = Tree String

linebreak :: Doc
linebreak = text "\n"

formatTree :: PendingTree -> Doc
formatTree t = formatSubTree 0 0 t

formatSubTrees :: Int -> Int -> [PendingTree] -> Doc
formatSubTrees _ _ [] = empty
formatSubTrees eDep bDep [t @ (Node _ [])] =
  treeNone eDep <> treeBranch bDep <> text " └> " <> formatSubTree eDep bDep t
formatSubTrees eDep bDep [t] =
  treeNone eDep <> treeBranch bDep <> text " └─ " <> formatSubTree (eDep + 1) bDep t
formatSubTrees eDep bDep ((Node s []):ts) =
  (treeNone eDep <> treeBranch bDep <> text " ├> " <> formatSubTree eDep bDep (Node s [])) <> linebreak <>
  (formatSubTrees eDep bDep ts)
formatSubTrees eDep bDep (t:ts) =
  (treeNone eDep <> treeBranch bDep <> text " ├─ " <> formatSubTree eDep (bDep + 1) t) <> linebreak <>
  (formatSubTrees eDep bDep ts)

formatSubTree :: Int -> Int -> PendingTree -> Doc
formatSubTree _ _ (Node s []) =
  text s
formatSubTree eDep bDep (Node s ts) =
  text s <> linebreak <> formatSubTrees eDep bDep ts

treeBranch :: Int -> Doc
treeBranch level = hcat $ replicate level (text " │ ")

treeNone :: Int -> Doc
treeNone level = hcat $ replicate level (text "   ")

size :: PendingTree -> Int
size (Node _ []) = 1
size (Node _ (t @ (_:_))) = sum $ map size t

limitPendingTree :: Int -> PendingTree -> PendingTree
limitPendingTree _ (Node t []) = (Node t [])
limitPendingTree 0 (Node t sub) = (Node (t ++ showTasks (sum $ map size sub)) [])
limitPendingTree n (Node t sub) = (Node t (map (limitPendingTree (n-1)) sub))

showTasks :: Int -> String
showTasks 1 = " (1 task)"
showTasks n = " (" ++ (show n) ++ " tasks)"

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
        Nothing  -> Just $ render $ vcat $ map formatTree t
        (Just i) -> Just $ render $ vcat $ map (formatTree . (limitPendingTree i)) t
