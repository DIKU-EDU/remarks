{-# LANGUAGE DeriveGeneric #-}

module Pending ( findPending ) where

import Ast

import Data.Tree
import Text.PrettyPrint

type PendingTree = Tree String

linebreak :: Doc
linebreak = text "\n"

formatTree :: PendingTree -> Doc
formatTree t = formatSubTree [] t

formatSubTrees :: [Bool] -> [PendingTree] -> Doc
formatSubTrees _ [] = empty
formatSubTrees depth [t @ (Node _ [])] =
  mapTreeBranch depth <> text " |> " <> formatSubTree depth t
formatSubTrees depth [t] =
  mapTreeBranch depth <> text " |- " <> formatSubTree (depth ++ [False]) t
formatSubTrees depth ((Node s []):ts) =
  (mapTreeBranch depth <> text " |> " <> formatSubTree depth (Node s [])) <> linebreak <>
  (formatSubTrees depth ts)
formatSubTrees depth (t:ts) =
  (mapTreeBranch depth <> text " |- " <> formatSubTree (depth ++ [True]) t) <> linebreak <>
  (formatSubTrees depth ts)

formatSubTree :: [Bool] -> PendingTree -> Doc
formatSubTree _ (Node s []) =
  text s
formatSubTree depth (Node s ts) =
  text s <> linebreak <> formatSubTrees depth ts

mapTreeBranch :: [Bool] -> Doc
mapTreeBranch = hcat . (map treeBranch)

treeBranch :: Bool -> Doc
treeBranch True  = text " | "
treeBranch False = text "   "

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
