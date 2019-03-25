module Export.Generic where

import Ast
import Prelude hiding ((<>)) -- use (<>) from Text.PrettyPrint
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

isIntegral :: Int -> Bool
isIntegral x = 0 == x `mod` 100

pointsDoc :: Maybe Int -> Doc
pointsDoc Nothing = text "*"
pointsDoc (Just v) | isIntegral v = int (v `div` 100)
pointsDoc (Just v) = int (v `div` 100) <> text "." <> int ((v `mod` 100) `div` 10) <> ppCent (v `mod` 10)
  where
    ppCent 0 = empty
    ppCent n = int n


propertyExpDoc :: PropertyExp -> Doc
propertyExpDoc (Lookup (index, name)) =
  brackets $ int index <> text "." <> text name
propertyExpDoc (Value value) = text value
propertyExpDoc (Num value) = pointsDoc $ Just value
propertyExpDoc (ArithFun fun str) = propertyArithFunDoc fun <> (parens $ text str)

propertyArithFunDoc :: PropertyArithFun -> Doc
propertyArithFunDoc Sum = text "sum"
propertyArithFunDoc Min = text "min"
propertyArithFunDoc Max = text "max"

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
summary _ (Bonus (h, prop, _)) =
  (Bonus (h, prop, []))
summary 0 (Judgement (h, _, p, _)) =
  Judgement (h, [], p, [])
summary depth (Judgement (h, _, _, js)) =
  Judgement (h, [], [], map (summary $ depth - 1) js)

-- For retrieval of property values

lookupProperty :: String -> Judgement -> Maybe Doc
lookupProperty name (Judgement (_, properties, _, _)) =
  case (lookup name (map (\(Property (n,v)) -> (n,v)) properties)) of
    Nothing -> Nothing
    Just(value) -> pure $ propertyExpDoc value
lookupProperty name (Bonus (_, properties, _)) =
  case (lookup name (map (\(Property (n,v)) -> (n,v)) properties)) of
    Nothing -> Nothing
    Just(value) -> pure $ propertyExpDoc value

lookupTotal :: Judgement -> Doc
lookupTotal j =
  case lookupProperty "Total" j of
    Nothing -> error $ "Total not found. Please report!"
    Just d  -> d

lookupMaxPoints :: Judgement -> Doc
lookupMaxPoints j =
  case lookupProperty "MaxPoints" j of
    Nothing -> error "MaxPoint not found. Please report!"
    Just d  -> d

lookupTitle :: Judgement -> Doc
lookupTitle j =
  case lookupProperty "Title" j of
    Nothing -> error "Title not found. Please report!"
    Just d  -> d

getTotal :: Judgement -> String
getTotal = render . lookupTotal

getTitle :: Judgement -> String
getTitle = render . lookupTitle

getMaxPoints :: Judgement -> String
getMaxPoints = render . lookupMaxPoints
