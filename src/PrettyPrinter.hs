module PrettyPrinter (ppJs) where

import Ast
import Export.Generic

import Text.PrettyPrint
import Data.List (intersperse)

ppJs :: [Judgement] -> String
ppJs = render . vcat . intersperse (text "") . map (formatJudgement 1)

formatJudgement :: Int -> Judgement -> Doc
formatJudgement level (Bonus (points, comments)) =
  (text $ replicate level '#') <+> text "Bonus" <> colon <+> text "+" <>
    pointsDoc points
formatJudgement level (Judgement (header, properties, comments, judgements)) =
  formatHeader level header $+$
  (nest 2 $ vcat $ map formatProperty properties) $+$
  (nest 2 $ vcat $ map formatComment comments) $+$
  (vcat $ map (formatJudgement (level + 1)) judgements)

formatHeader :: Int -> Header -> Doc
formatHeader level (Header (title, point, maxPoints)) =
  (text $ replicate level '#') <+> text title <> colon <>
    pointsDoc point <> text "/" <> pointsDoc maxPoints

formatProperty :: Property -> Doc
formatProperty (Property (name, value)) =
  colon <> text name <> colon <+> formatPropertyExp value

formatPropertyExp :: PropertyExp -> Doc
formatPropertyExp (Lookup (index, name)) =
  brackets $ int index <> text "." <> text name
formatPropertyExp (Value value) = text value
formatPropertyExp (Num double) = pointsDoc double

formatComment :: Comment -> Doc
formatComment (Comment (mood, commentParts)) =
  formatMood mood <+> (vcat $ map formatCommentPart commentParts)

formatMood :: Mood -> Doc
formatMood Positive  = text "+"
formatMood Negative  = text "-"
formatMood Neutral   = text "*"
formatMood Impartial = text "?"

formatCommentPart :: CommentPart -> Doc
formatCommentPart (CommentStr string)  = text string
formatCommentPart (CommentCmt comment) = nest 2 $ formatComment comment
