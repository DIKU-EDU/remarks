{-# LANGUAGE DeriveGeneric #-}

module Pending (findPending, findStatus) where

import Ast
-- use (<>) from Text.PrettyPrint
import Text.PrettyPrint
import Prelude hiding ((<>))

data PendingTree
  = Node String Int Bool [PendingTree]
  -- Name, Number of Remarks, Is pending, Subtree
  deriving (Eq, Show)

data FormatTree
  = TEmpty FormatTree
  | TSpace FormatTree
  | TBranch FormatTree
  | TNode
  | TLeaf
  | TQuest
  deriving (Eq, Show)

showTree :: FormatTree -> Doc
showTree (TEmpty t) = showTree t
showTree (TBranch t) = text " | " <> showTree t
showTree (TSpace t) = text "   " <> showTree t
showTree TNode = text " |- "
showTree TLeaf = text " |> "
showTree TQuest = text " |? "

linebreak :: Doc
linebreak = text "\n"

isLeaf :: PendingTree -> Bool
isLeaf (Node _ _ _ []) = True
isLeaf (Node _ _ _ (_ : _)) = False

isTrimmed :: PendingTree -> Bool
isTrimmed (Node "" _ _ _) = True
isTrimmed _ = False

formatTree :: PendingTree -> Doc
formatTree (Node s i b pt) = formatSubTree (TSpace) (Node ("  " ++ s) i b pt)

formatSubTrees :: (FormatTree -> FormatTree) -> [PendingTree] -> Doc
formatSubTrees _ [] = empty
formatSubTrees ft (t : ts) | isTrimmed t = formatSubTrees ft ts
formatSubTrees ft [t]
  | isLeaf t =
      linebreak <> showTree (ft TLeaf) <> formatSubTree (ft . TSpace) t
formatSubTrees ft [t] =
  linebreak <> showTree (ft TNode) <> formatSubTree (ft . TSpace) t
formatSubTrees ft (t : ts)
  | isLeaf t =
      linebreak
        <> showTree (ft TLeaf)
        <> formatSubTree (ft . TBranch) t
        <> formatSubTrees ft ts
formatSubTrees ft (t : ts) =
  linebreak
    <> showTree (ft TNode)
    <> formatSubTree (ft . TBranch) t
    <> formatSubTrees ft ts

formatSubTree :: (FormatTree -> FormatTree) -> PendingTree -> Doc
-- formatSubTree ft (Node "" _ _ _)  = empty
formatSubTree ft (Node s cs _ ts) = text s <> formatTreeRemarks ft cs <> formatSubTrees ft ts

formatTreeRemarks :: (FormatTree -> FormatTree) -> Int -> Doc
formatTreeRemarks _ 0 = empty
formatTreeRemarks ft cs =
  linebreak <> showTree (ft TQuest) <> text (makePlural cs "impartial remark")

size :: PendingTree -> ((Int, Int), Int) -- (Pending, Total), Remarks
size (Node _ cs True []) = ((1, 1), cs)
size (Node _ cs False []) = ((0, 1), cs)
size (Node _ cs _ pt) = foldl tupAdd ((0, 0), cs) $ map size pt
  where
    tupAdd :: ((Int, Int), Int) -> ((Int, Int), Int) -> ((Int, Int), Int)
    tupAdd a b = (((fst . fst) a + (fst . fst) b, (snd . fst) a + (snd . fst) b), snd a + snd b)

limitPendingTree :: Int -> PendingTree -> PendingTree
limitPendingTree _ (Node s 0 b []) = Node s 0 b []
limitPendingTree 0 (t@(Node s _ b _)) = Node (s ++ showTasks (size t)) 0 b []
limitPendingTree _ (t@(Node s cs False _)) = Node (s ++ showTasks (size t)) cs False []
limitPendingTree n (Node s cs True ts) = Node s cs True (map (limitPendingTree (n - 1)) ts)

trimPendingTree :: PendingTree -> PendingTree
trimPendingTree (Node _ _ False _) = Node "" 0 False []
trimPendingTree t@(Node _ _ True []) = t
trimPendingTree (Node s cs True ts) = Node s cs True (map trimPendingTree ts)

showTasks :: ((Int, Int), Int) -> String
showTasks ((n, s), 0) = " : " ++ show n ++ " of " ++ show s ++ " " ++ makePlural n "task" ++ " (" ++ showPercentage n s ++ ")"
showTasks ((0, _), m) = " : " ++ show m ++ " " ++ makePlural m "remark" ++ ")"
showTasks ((n, s), m) = " : " ++ show n ++ " of " ++ show s ++ " " ++ makePlural n "task" ++ " (" ++ showPercentage n s ++ ")" ++ " and " ++ show m ++ " " ++ makePlural m "remark"

showPercentage :: Int -> Int -> String
showPercentage n s = show (div (n * 100) (s)) ++ "%"

makePlural :: Int -> String -> String
makePlural 1 s = s
makePlural _ s = s ++ "s"

pendingJudgement :: Judgement -> [PendingTree]
pendingJudgement (Bonus (_, _, cs)) =
  case countImpartials cs of
    0 -> []
    n -> [Node "Bonus" n False []]
pendingJudgement (Feedback (_, _)) = []
pendingJudgement (Judgement (Header (t, NotGiven, _), _, cs, [])) =
  [Node t (countImpartials cs) True []]
pendingJudgement (Judgement (Header (t, _, _), _, cs, [])) =
  [Node t (countImpartials cs) False []]
pendingJudgement (Judgement (Header (t, _, _), _, cs, subJs)) =
  [Node t (countImpartials cs) has_pending sub_pending]
  where
    sub_pending = concatMap pendingJudgement subJs
    has_pending = or $ map (\(Node _ _ b _) -> b) sub_pending

countImpartials :: [Remark] -> Int
countImpartials = sum . (map countImpartial)

countImpartial :: Remark -> Int
countImpartial (Remark (Impartial, cps)) = 1 + (sum $ map countImpartialCP cps)
countImpartial (Remark (_, cps)) = sum $ map countImpartialCP cps

countImpartialCP :: RemarkPart -> Int
countImpartialCP (RemarkStr _) = 0
countImpartialCP (RemarkCmt c) = countImpartial c

findPending :: Maybe Int -> [Judgement] -> Maybe (String)
findPending detailLevel js =
  case (concatMap pendingJudgement js) of
    [] -> Nothing
    t ->
      case detailLevel of
        Nothing -> Just $ render $ vcat $ map (formatTree . trimPendingTree) t
        (Just i) -> Just $ render $ vcat $ map (formatTree . trimPendingTree . (limitPendingTree i)) t

findStatus :: Maybe Int -> [Judgement] -> Maybe (String)
findStatus detailLevel js =
  case (concatMap pendingJudgement js) of
    [] -> Nothing
    t ->
      case detailLevel of
        Nothing -> Just $ render $ vcat $ map formatTree t
        (Just i) -> Just $ render $ vcat $ map (formatTree . (limitPendingTree i)) t
