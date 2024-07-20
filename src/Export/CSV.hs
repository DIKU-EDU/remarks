module Export.CSV (csvRemarks) where

import Ast
import Data.List (intersperse)
import Export.Generic
import Invalid
import Text.PrettyPrint

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
        Nothing -> pure empty
        -- Nothing  -> Left $ PropertyNotFound p j
        (Just v) -> pure v
