module Export.Generic where

import Ast
import Text.PrettyPrint

import Data.List (intersperse, transpose)

data Table
  = Rows [Row]
  | Cols [Col]
  deriving (Eq, Show)

type Row = [Elem]
type Col = [Elem]
type Elem = String

reformat :: Table -> Table
reformat (Rows t) = Cols $ transpose t
reformat (Cols t) = Rows $ transpose t

transp :: Table -> Table
transp (Rows t) = Cols t
transp (Cols t) = Rows t

toCSV :: String -> Table -> String
toCSV s (t@(Cols _)) = toCSV s $ reformat t
toCSV s (Rows rs) = concat $ intersperse "\n" $ map formatRow rs
  where
    formatRow r  = concat $ intersperse s $ map formatElem r
    formatElem e = e -- Currently only string to string

toHTML :: Table -> String
toHTML (t@(Cols _)) = toHTML $ reformat t
toHTML (Rows rs) = "<table>" ++ (concatMap formatRow rs) ++ "</table>"
  where
    formatRow r  = "<tr>" ++ (concatMap formatElem r) ++ "</tr>"
    formatElem e = "<td>" ++ e ++ "</td>"

isIntegral :: Double -> Bool
isIntegral x = x == fromInteger (round x)

pointsDoc :: Double -> Doc
pointsDoc v | isInfinite v = text "*"
pointsDoc v | isNaN v = empty
pointsDoc v | isIntegral v = integer (round v)
pointsDoc v = double v

formatPropertyExp :: PropertyExp -> Doc
formatPropertyExp (Lookup (index, name)) =
  brackets $ int index <> text "." <> text name
formatPropertyExp (Value value) = text value
formatPropertyExp (Num value) = pointsDoc value
formatPropertyExp (Sum str) = text "sum" <> (parens $ text str)

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

-- For retrieval of property values

lookupProperty :: String -> Judgement -> Maybe Doc
lookupProperty name (Judgement (_, properties, _, _)) =
  case (lookup name (map (\(Property (n,v)) -> (n,v)) properties)) of
    Nothing -> Nothing
    Just(value) -> pure $ formatPropertyExp value
lookupProperty _ _ = Nothing -- Bonus does not have properties

lookupTotal :: Judgement -> Doc
lookupTotal j =
  case lookupProperty "Total" j of
    Nothing -> error "This should not occur. Please report!"
    Just d  -> d

lookupMaxPoints :: Judgement -> Doc
lookupMaxPoints j =
  case lookupProperty "MaxPoints" j of
    Nothing -> error "This should not occur. Please report!"
    Just d  -> d

lookupTitle :: Judgement -> Doc
lookupTitle j =
  case lookupProperty "Title" j of
    Nothing -> error "This should not occur. Please report!"
    Just d  -> d

getTotal :: Judgement -> String
getTotal = render . lookupTotal

getTitle :: Judgement -> String
getTitle = render . lookupTitle

getMaxPoints :: Judgement -> String
getMaxPoints = render . lookupMaxPoints
