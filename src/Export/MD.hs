module Export.MD (mdRemarks) where

import Ast
import Export.Generic

import Text.PrettyPrint
import Data.List (intersperse)

mdRemarks :: [Judgement] -> String
mdRemarks = render . vcat . intersperse (text "") . map (formatJudgement 1)

formatJudgement :: Int -> Judgement -> Doc
formatJudgement depth (Bonus (p, _)) =
  (text $ replicate depth '#') <+> text "Bonus" <> colon <+> text "+" <>
    pointsDoc p
formatJudgement depth (j @ (Judgement (_, _, comments, judgements))) =
  formatHeader depth j $+$
  (nest 2 $ vcat $ map formatComment comments) $+$
  text "" $+$
  (vcat $ map (formatJudgement (depth + 1)) judgements)

formatHeader :: Int -> Judgement -> Doc
formatHeader depth j =
  (text $ replicate depth '#') <+> getTitle j <> colon <> space <>
    getTotal j <> text "/" <> getMax j

formatComment :: Comment -> Doc
formatComment (Comment (mood, commentParts)) =
  text "*" <+> formatMood mood <+> (vcat $ map formatCommentPart commentParts)

formatMood :: Mood -> Doc
formatMood Positive  = text "(+)"
formatMood Negative  = text "(-)"
formatMood Neutral   = text ""
formatMood Impartial = text "(?)"

formatCommentPart :: CommentPart -> Doc
formatCommentPart (CommentStr string)  = text string
formatCommentPart (CommentCmt comment) = nest 2 $ formatComment comment

getTotal :: Judgement -> Doc
getTotal j =
  case lookupProperty "Total" j of
    Nothing -> error "This should not occur. Please report!"
    Just d  -> d

getMax :: Judgement -> Doc
getMax j =
  case lookupProperty "MaxPoints" j of
    Nothing -> error "This should not occur. Please report!"
    Just d  -> d

getTitle :: Judgement -> Doc
getTitle j =
  case lookupProperty "Title" j of
    Nothing -> error "This should not occur. Please report!"
    Just d  -> d

