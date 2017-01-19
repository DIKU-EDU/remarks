module Export.Generic where

import Ast
import Text.PrettyPrint

isIntegral :: Double -> Bool
isIntegral x = x == fromInteger (round x)

pointsDoc :: Double -> Doc
pointsDoc v | isNaN v = empty
pointsDoc v | isIntegral v = integer (round v)
pointsDoc v = double v

unify :: Judgement -> Judgement -> Maybe Judgement
unify
  (Judgement (lh, _, lcs, []))
  (Judgement (rh, rps, rcs, _))
        | lh == rh && lcs == rcs = do
    pure $ Judgement (lh, rps, lcs, [])
unify
  (Judgement (lh, _, lcs, ljs))
  (Judgement (rh, rps, rcs, rjs))
        | lh == rh && lcs == rcs = do
    newJs <- mapM (uncurry unify) (zip ljs rjs)
    pure $ Judgement (lh, rps, lcs, newJs)
unify (Bonus l) (Bonus r) | l == r =
    pure $ (Bonus l)
unify _ _ = Nothing

summary :: Word -> Judgement -> Judgement
summary _ (Bonus (p, _)) =
  (Bonus (p, []))
summary 0 (Judgement (h, _, _, _)) =
  Judgement (h, [], [], [])
summary depth (Judgement (h, _, _, js)) =
  Judgement (h, [], [], map (summary $ depth - 1) js)
