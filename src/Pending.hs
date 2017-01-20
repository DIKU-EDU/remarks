{-# LANGUAGE DeriveGeneric #-}

module Pending ( findPending ) where

import Ast

import Data.Tree
import Text.PrettyPrint

type PendingTree = Tree String

data FormatTree
  = TEmpty  FormatTree
  | TSpace  FormatTree
  | TBranch FormatTree
  | TNode
  | TLeaf

showTree :: FormatTree -> Doc
showTree (TEmpty t)  = showTree t
showTree (TBranch t) = text " | " <> showTree t
showTree (TSpace t)  = text "   " <> showTree t
showTree TNode       = text " |- "
showTree TLeaf       = text " |> "

linebreak :: Doc
linebreak = text "\n"

formatTree :: PendingTree -> Doc
formatTree t = formatSubTree (TEmpty) t

formatSubTrees :: (FormatTree -> FormatTree) -> [PendingTree] -> Doc
formatSubTrees _ [] = empty
formatSubTrees ft [(Node s [])] =
  showTree (ft TLeaf) <> text s
formatSubTrees ft [t] =
  showTree (ft TNode) <> formatSubTree (ft . TSpace) t
formatSubTrees ft ((Node s []):ts) =
  showTree (ft TLeaf) <> text s <> linebreak <>
  (formatSubTrees ft ts)
formatSubTrees ft (t:ts) =
  showTree (ft TNode) <> formatSubTree (ft . TBranch) t <> linebreak <>
  (formatSubTrees ft ts)

formatSubTree :: (FormatTree -> FormatTree) -> PendingTree -> Doc
formatSubTree _ (Node s []) =
  text s
formatSubTree tree (Node s ts) =
  text s <> linebreak <> formatSubTrees tree ts

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
