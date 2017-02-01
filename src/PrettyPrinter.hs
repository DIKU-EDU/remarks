module PrettyPrinter (ppJ_d, ppJs, ppPoints, ppComments) where

import Ast
import Export.Generic

import Text.PrettyPrint
import Data.List (intersperse)

ppJ_d :: Int -> Judgement -> String
ppJ_d d = render . (formatJudgement (d + 1))

ppJs :: [Judgement] -> String
ppJs = render . vcat . intersperse (text "") . map (formatJudgement 1)

ppPoints :: Double -> String
ppPoints = render . pointsDoc

ppComments :: [Comment] -> String
ppComments = render . vcat . (map formatComment)

formatJudgement :: Int -> Judgement -> Doc
formatJudgement depth (Bonus (points, _)) =
  (text $ replicate depth '#') <+> text "Bonus" <> colon <+> text "+" <>
    pointsDoc points
formatJudgement depth (Judgement (header, properties, comments, judgements)) =
  formatHeader depth header $+$
  (nest 2 $ vcat $ map formatProperty properties) $+$
  (nest 2 $ vcat $ map formatComment comments) $+$
  (vcat $ map (formatJudgement (depth + 1)) judgements)

formatHeader :: Int -> Header -> Doc
formatHeader depth (Header (title, point, maxPoints)) =
  (text $ replicate depth '#') <+> text title <> colon <> space <>
    pointsDoc point <> text "/" <> pointsDoc maxPoints

formatProperty :: Property -> Doc
formatProperty (Property (name, value)) =
  colon <> text name <> colon <+> formatPropertyExp value

formatComment :: Comment -> Doc
formatComment (Comment (mood, commentParts)) =
  formatMood mood <+> (vcat $ map formatCommentPart commentParts)

formatMood :: Mood -> Doc
formatMood Positive  = text "+"
formatMood Negative  = text "-"
formatMood Neutral   = text "*"
formatMood Impartial = text "?"
formatMood Warning   = text "!"

formatCommentPart :: CommentPart -> Doc
formatCommentPart (CommentStr string)  = text string
formatCommentPart (CommentCmt comment) = nest 2 $ formatComment comment
