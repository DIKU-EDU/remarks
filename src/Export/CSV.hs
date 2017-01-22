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
    mapfun j p = 
      case lookupProperty p j of
        Nothing  -> Left $ PropertyNotFound p j
        (Just v) -> pure v
