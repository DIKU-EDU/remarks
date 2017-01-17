module PrettyPrinter (ppJs) where

import Ast

import Text.PrettyPrint
import Data.List (intersperse)

ppJs :: [Judgement] -> String
ppJs = render . vcat . intersperse (text "") . map (formatJudgement 1)

formatJudgement :: Int -> Judgement -> Doc
formatJudgement level (Bonus (points, comments)) =
  (text $ replicate level '#') <+> text "Bonus" <> colon <+> text "+" <>
    pointsDoc points
formatJudgement level (Judgement (header, comments, judgements)) =
  formatHeader level header $+$
  (nest 2 $ vcat $ map (formatComment) comments) $+$
  (vcat $ map (formatJudgement (level + 1)) judgements)

isIntegral :: Double -> Bool
isIntegral x = x == fromInteger (round x)

pointsDoc :: Double -> Doc
pointsDoc v | isNaN v = empty
pointsDoc v | isIntegral v = integer (round v)
pointsDoc v = double v

formatHeader :: Int -> Header -> Doc
formatHeader level (Header (title, point, maxPoints)) =
  (text $ replicate level '#') <+> text title <> colon <> space <>
    pointsDoc point <> text "/" <> pointsDoc maxPoints

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
