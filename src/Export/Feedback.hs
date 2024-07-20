module Export.Feedback (FeedbackOpts (..), feedbackRemarks) where

import Ast
import Export.Generic
import Text.PrettyPrint
import Prelude hiding ((<>))

newtype FeedbackOpts = FeedbackOpts
  { withPoints :: Bool
  }

feedbackRemarks :: FeedbackOpts -> [Judgement] -> String
feedbackRemarks opts = render . vcat . map (formatJudgement opts 1)

formatJudgement :: FeedbackOpts -> Int -> Judgement -> Doc
formatJudgement _ _ (Feedback (_, t)) =
  case length t of
    0 -> empty
    _ -> text t $+$ text ""
formatJudgement _ _ (Bonus (_, _, _)) = empty
formatJudgement opts depth (j@(Judgement (_, _, _, judgements))) =
  case isEmpty subj of
    True -> empty
    False ->
      formatHeader (withPoints opts) depth j
        $+$ text ""
        $+$ subj
  where
    subj = vcat $ map (formatJudgement opts (depth + 1)) judgements

formatHeader :: Bool -> Int -> Judgement -> Doc
formatHeader showPoints depth j =
  let points =
        if showPoints
          then
            colon
              <> space
              <> lookupTotal j
              <> text "/"
              <> lookupMaxPoints j
          else empty
   in (text $ replicate depth '#') <+> lookupTitle j <> points
