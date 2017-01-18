module Export.CSV (csvRemarks) where

import Ast
import Export.Generic

import Text.PrettyPrint
import Data.List (intersperse)

csvRemarks :: String -> [String] -> [Judgement] -> Doc
csvRemarks delimiter propertyList judgements = 
  (hcat $ (intersperse (text delimiter)) $ map text propertyList) $$
  (vcat $ map (formatJudgement delimiter propertyList) judgements)

formatJudgement :: String -> [String] -> Judgement -> Doc
formatJudgement delimiter properties judgement = 
  hcat $ (intersperse (text delimiter)) $ map (mapfun judgement) properties
  where
    mapfun = flip lookupProperty 

lookupProperty :: String -> Judgement -> Doc
lookupProperty name (Judgement (_, properties, _, _)) = 
  case (lookup name (map (\(Property (n,v)) -> (n,v)) properties)) of
    Nothing -> error "Property not found."
    Just(value) -> formatPropertyExp value

formatPropertyExp :: PropertyExp -> Doc
formatPropertyExp (Lookup (index, name)) =
  brackets $ int index <> text "." <> text name
formatPropertyExp (Value value) = text value
formatPropertyExp (Num value) = pointsDoc value
