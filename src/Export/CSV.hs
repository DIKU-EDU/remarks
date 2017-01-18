module Export.CSV (csvRemarks) where

import Ast
import Invalid
import Export.Generic

import Text.PrettyPrint
import Data.List (intersperse)

csvRemarks :: String -> [String] -> [Judgement] -> Either Invalid Doc
csvRemarks delimiter propertyList judgements = do
  doc <- mapM (formatJudgement delimiter propertyList) judgements
  pure ((hcat $ (intersperse (text delimiter)) $ map text propertyList) $$ (vcat $ doc))


formatJudgement :: String -> [String] -> Judgement -> Either Invalid Doc
formatJudgement delimiter properties judgement = do
  doc <- mapM (mapfun judgement) properties
  pure $ hcat $ (intersperse (text delimiter)) doc
  where
    mapfun = flip lookupProperty

lookupProperty :: String -> Judgement -> Either Invalid Doc
lookupProperty name (Judgement (_, properties, _, _)) =
  case (lookup name (map (\(Property (n,v)) -> (n,v)) properties)) of
    Nothing -> Left $ PropertyNotFound name
    Just(value) -> pure $ formatPropertyExp value

formatPropertyExp :: PropertyExp -> Doc
formatPropertyExp (Lookup (index, name)) =
  brackets $ int index <> text "." <> text name
formatPropertyExp (Value value) = text value
formatPropertyExp (Num value) = pointsDoc value
