{-# LANGUAGE DeriveGeneric #-}

module MergeAsts ( mergeProps ) where

import Ast

mergeProps :: [Judgement] -> [Judgement] -> [Judgement]
mergeProps js jsp =
  map fun js
  where
    fun j@(Judgement (h,_,_,_)) = mergeMaybeJudgement j (findJudgement h jsp)
    fun j = j

findJudgement :: Header -> [Judgement] -> Maybe Judgement
findJudgement _ [] = Nothing
findJudgement (Header (h, _, _)) ((j@(Judgement (Header (hp,_,_), _, _, _))):_) | h == hp = Just j
findJudgement (Header (_, _, _)) ((j@(Judgement (Header (hp,_,_), _, _, _))):_) | "*" == hp = Just j
findJudgement h (_:js) = findJudgement h js

mergeMaybeJudgement :: Judgement -> (Maybe Judgement) -> Judgement
mergeMaybeJudgement j Nothing   = j
mergeMaybeJudgement j (Just jp) = mergeJudgement j jp

mergeJudgement :: Judgement -> Judgement -> Judgement
mergeJudgement (Judgement (header, p1, comments, subjs)) (Judgement (_, p2, [], subpjs)) =
  Judgement (header, p1++p2, comments, (mergeProps subjs subpjs))
mergeJudgement j _ = j  -- If there is something with bonus, just not add anything
