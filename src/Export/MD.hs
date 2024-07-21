module Export.MD (mdRemarks) where

import Ast
-- use (<>) from Text.PrettyPrint

import Data.List (intersperse)
import Export.Generic
import Text.PrettyPrint
import Prelude hiding ((<>))

mdRemarks :: [Judgement] -> String
mdRemarks = render . vcat . intersperse (text "") . map (formatJudgement 1)

formatJudgement :: Int -> Judgement -> Doc
formatJudgement depth (j@(Bonus _)) =
  (text $ replicate depth '#')
    <+> text "Bonus"
    <> colon
    <+> text "+"
    <> lookupTotal j
formatJudgement depth (Feedback (_, t)) =
  (text $ replicate depth '#') <+> text "Feedback" <> colon
    $+$ text "+"
      <> text t
formatJudgement depth (j@(Judgement (_, _, remarks, judgements))) =
  formatHeader depth j
    $+$ (nest 2 $ vcat $ map formatRemark remarks)
    $+$ text ""
    $+$ (vcat $ map (formatJudgement (depth + 1)) judgements)

formatHeader :: Int -> Judgement -> Doc
formatHeader depth j =
  (text $ replicate depth '#')
    <+> lookupTitle j
    <> colon
    <> space
    <> lookupTotal j
    <> text "/"
    <> lookupMaxPoints j

formatRemark :: Remark -> Doc
formatRemark (Remark (mood, remarkParts)) =
  text "*" <+> formatMood mood <+> (vcat $ map formatRemarkPart remarkParts)

formatMood :: Mood -> Doc
formatMood Positive = text "(+)"
formatMood Negative = text "(-)"
formatMood Neutral = text ""
formatMood Mixed = text "(~)"
formatMood Impartial = text "(?)"
formatMood Warning = text "(!)"

formatRemarkPart :: RemarkPart -> Doc
formatRemarkPart (RemarkStr string) = text string
formatRemarkPart (RemarkCmt remark) = nest 2 $ formatRemark remark
