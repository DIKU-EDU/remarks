{-# LANGUAGE DeriveGeneric #-}

module Pending ( findPending ) where

import Ast

import Text.PrettyPrint

data PendingTree
  = Node String Int [PendingTree]

data FormatTree
  = TEmpty  FormatTree
  | TSpace  FormatTree
  | TBranch FormatTree
  | TNode
  | TLeaf
  | TQuest

showTree :: FormatTree -> Doc
showTree (TEmpty t)  = showTree t
showTree (TBranch t) = text " | " <> showTree t
showTree (TSpace t)  = text "   " <> showTree t
showTree TNode       = text " |- "
showTree TLeaf       = text " |> "
showTree TQuest      = text " |? "

linebreak :: Doc
linebreak = text "\n"

isLeaf :: PendingTree -> Bool
isLeaf (Node _ _ [])    = True
isLeaf (Node _ _ (_:_)) = False

formatTree :: PendingTree -> Doc
formatTree t = formatSubTree (TEmpty) t

formatSubTrees :: (FormatTree -> FormatTree) -> [PendingTree] -> Doc
formatSubTrees _ [] = empty
formatSubTrees ft [t] | isLeaf t =
  linebreak <> showTree (ft TLeaf) <> formatSubTree (ft . TSpace) t
formatSubTrees ft [t] =
  linebreak <> showTree (ft TNode) <> formatSubTree (ft . TSpace) t
formatSubTrees ft (t:ts) | isLeaf t =
  linebreak <> showTree (ft TLeaf) <> formatSubTree (ft . TBranch) t <>
  formatSubTrees ft ts
formatSubTrees ft (t:ts) =
  linebreak <> showTree (ft TNode) <> formatSubTree (ft . TBranch) t <>
  formatSubTrees ft ts

formatSubTree :: (FormatTree -> FormatTree) -> PendingTree -> Doc
formatSubTree ft (Node s cs ts) = text s <> formatTreeComments ft cs <> formatSubTrees ft ts

formatTreeComments :: (FormatTree -> FormatTree) -> Int -> Doc
formatTreeComments _ 0 = empty
formatTreeComments ft cs =
  linebreak <> showTree (ft TQuest) <> text (makePlural cs "impartial comment")

size :: PendingTree -> (Int, Int)
size (Node _ 0 []) = (1, 0)
size (Node _ cs []) = (1, cs)
size (Node _ cs pt) = foldl tupAdd (0, cs) $ map size pt
  where tupAdd a b = (fst a + fst b, snd a + snd b)

limitPendingTree :: Int -> PendingTree -> PendingTree
limitPendingTree _    (Node s 0 [])  = Node s 0 []
limitPendingTree 0 (t@(Node s _ _))  = Node (s ++ showTasks (size t)) 0 []
limitPendingTree n    (Node s cs ts) = Node s cs (map (limitPendingTree (n-1)) ts)

showTasks :: (Int, Int) -> String
showTasks (n, 0) = " (" ++ makePlural n "task" ++ ")"
showTasks (0, m) = " (" ++ makePlural m "comment" ++ ")"
showTasks (n, m) = " (" ++ makePlural n "task" ++ " and " ++ makePlural m "comment" ++ ")"

makePlural :: Int -> String -> String
makePlural 0 s = "0 " ++ s ++ "s"
makePlural 1 s = "1 " ++ s
makePlural n s = show n ++ " " ++ s ++ "s"

pendingJudgement :: Judgement -> [PendingTree]
pendingJudgement (Bonus (_, cs)) =
  case countImpartials cs of
    0 -> []
    n -> [Node "Bonus" n []]
pendingJudgement (Judgement (Header (t, p, _), _, cs, [])) | isInfinite p =
  [Node t (countImpartials cs) []]
pendingJudgement (Judgement (Header (t, _, _), _, cs, subJs)) =
  case (concatMap pendingJudgement subJs) of
    [] ->
      case countImpartials cs of
        0 -> []
        n -> [Node t n []]
    sub  -> [Node t (countImpartials cs) sub]

countImpartials :: [Comment] -> Int
countImpartials = sum . (map countImpartial)

countImpartial :: Comment -> Int
countImpartial (Comment (Impartial, cps)) = 1 + (sum $ map countImpartialCP cps)
countImpartial (Comment (_, cps)) = sum $ map countImpartialCP cps

countImpartialCP :: CommentPart -> Int
countImpartialCP (CommentStr _) = 0
countImpartialCP (CommentCmt c) = countImpartial c

findPending :: Maybe Int -> [Judgement] -> Maybe(String)
findPending detailLevel js =
  case (concatMap pendingJudgement js) of
    [] -> Nothing
    t  ->
      case detailLevel of
        Nothing  -> Just $ render $ vcat $ map formatTree t
        (Just i) -> Just $ render $ vcat $ map (formatTree . (limitPendingTree i)) t
