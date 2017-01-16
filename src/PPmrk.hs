module PPmrk (toMrk) where

import Ast

import Text.PrettyPrint

toMrk :: [Judgement] -> String
toMrk js = render $ vcat $ map (formatJudgement 1) js

formatJudgement level (Judgement (header, comments, judgements)) =
  formatHeader level header $+$
  (nest 2 $ vcat $ map (formatComment) comments) $+$
  (vcat $ map (formatJudgement (level + 1)) judgements)

formatHeader level (Header (title, point, maxPoints)) =
  (text $ replicate level '#') <+> text title <> colon <+> double point <> text "/" <> double maxPoints

formatComment (Comment (mood, commentParts)) =
  formatMood mood <+> (vcat $ map formatCommentPart commentParts)

formatMood Positive  = text "+"
formatMood Negative  = text "-"
formatMood Neutral   = text "*"
formatMood Impartial = text "?"

formatCommentPart (CommentStr string)  = text string
formatCommentPart (CommentCmt comment) = nest 2 $ formatComment comment
