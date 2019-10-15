module Export.MD (mdRemarks) where

import Ast
import Export.Generic

import Prelude hiding ((<>)) -- use (<>) from Text.PrettyPrint
import Text.PrettyPrint
import Data.List (intersperse)

mdRemarks :: [Judgement] -> String
mdRemarks = render . vcat . intersperse (text "") . map (formatJudgement 1)

formatJudgement :: Int -> Judgement -> Doc
formatJudgement depth (j @ (Bonus _)) =
  (text $ replicate depth '#') <+> text "Bonus" <> colon <+> text "+" <>
    lookupTotal j
formatJudgement depth (Feedback (_, t)) =
  (text $ replicate depth '#') <+> text "Feedback" <> colon $+$ text "+" <>
    text t
formatJudgement depth (j @ (Judgement (_, _, comments, judgements))) =
  formatHeader depth j $+$
  (nest 2 $ vcat $ map formatComment comments) $+$
  text "" $+$
  (vcat $ map (formatJudgement (depth + 1)) judgements)

formatHeader :: Int -> Judgement -> Doc
formatHeader depth j =
  (text $ replicate depth '#') <+> lookupTitle j <> colon <> space <>
    lookupTotal j <> text "/" <> lookupMaxPoints j

formatComment :: Comment -> Doc
formatComment (Comment (mood, commentParts)) =
  text "*" <+> formatMood mood <+> (vcat $ map formatCommentPart commentParts)

formatMood :: Mood -> Doc
formatMood Positive  = text "(+)"
formatMood Negative  = text "(-)"
formatMood Neutral   = text ""
formatMood Mixed   = text "(~)"
formatMood Impartial = text "(?)"
formatMood Warning = text "(!)"

formatCommentPart :: CommentPart -> Doc
formatCommentPart (CommentStr string)  = text string
formatCommentPart (CommentCmt comment) = nest 2 $ formatComment comment
