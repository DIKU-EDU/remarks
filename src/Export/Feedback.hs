module Export.Feedback (feedbackRemarks) where

import Ast
import Export.Generic

import Prelude hiding ((<>))
import Text.PrettyPrint

feedbackRemarks :: [Judgement] -> String
feedbackRemarks = render . vcat . map (formatJudgement 1)

formatJudgement :: Int -> Judgement -> Doc
formatJudgement _ (Feedback (_, t)) = text t
formatJudgement _ (Bonus (_, _, _)) = empty
formatJudgement depth (j @ (Judgement (_, _, _, judgements))) =
  case isEmpty subj of
    True -> empty
    False -> formatHeader depth j $+$ text "" $+$ subj $+$ text ""
  where
    subj = vcat $ map (formatJudgement (depth + 1)) judgements

formatHeader :: Int -> Judgement -> Doc
formatHeader depth j =
  (text $ replicate depth '#') <+> lookupTitle j

